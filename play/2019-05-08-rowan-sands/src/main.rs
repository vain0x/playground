use rowan::{
    GreenNode, GreenNodeBuilder, SmolStr, SyntaxKind, SyntaxNode, TransparentNewType, TreeArc,
};

// '('
const L_PAREN: SyntaxKind = SyntaxKind(0);

const R_PAREN: SyntaxKind = SyntaxKind(1);
const WORD: SyntaxKind = SyntaxKind(2);
const WHITESPACE: SyntaxKind = SyntaxKind(3);
const ERROR: SyntaxKind = SyntaxKind(4);

const LIST: SyntaxKind = SyntaxKind(5);
const ATOM: SyntaxKind = SyntaxKind(6);
const ROOT: SyntaxKind = SyntaxKind(7);

#[derive(PartialEq, Eq, Hash)]
struct Sexp(SyntaxNode);

enum SexpKind<'a> {
    Atom(&'a Atom),
    List(&'a List),
}

impl Sexp {
    fn cast(node: &SyntaxNode) -> Option<&Self> {
        if Atom::cast(node).is_some() || List::cast(node).is_some() {
            Some(unsafe { std::mem::transmute(node) })
        } else {
            None
        }
    }
}

// repetitive:

#[derive(PartialEq, Eq, Hash)]
#[repr(transparent)]
struct Root(SyntaxNode);

unsafe impl TransparentNewType for Root {
    type Repr = SyntaxNode;
}

impl Root {
    #[allow(unused)]
    fn cast(node: &SyntaxNode) -> Option<&Self> {
        if node.kind() == ROOT.into() {
            Some(Self::from_repr(node))
        } else {
            None
        }
    }

    #[allow(unused)]
    fn to_owned(&self) -> TreeArc<Self> {
        let owned = self.0.to_owned();
        TreeArc::cast(owned)
    }
}

// repetitive:

#[derive(PartialEq, Eq, Hash)]
#[repr(transparent)]
struct Atom(SyntaxNode);

unsafe impl TransparentNewType for Atom {
    type Repr = SyntaxNode;
}

impl Atom {
    #[allow(unused)]
    fn cast(node: &SyntaxNode) -> Option<&Self> {
        if node.kind() == ATOM.into() {
            Some(Self::from_repr(node))
        } else {
            None
        }
    }

    #[allow(unused)]
    fn to_owned(&self) -> TreeArc<Self> {
        let owned = self.0.to_owned();
        TreeArc::cast(owned)
    }
}

// repetitive:

#[derive(PartialEq, Eq, Hash)]
#[repr(transparent)]
struct List(SyntaxNode);

unsafe impl TransparentNewType for List {
    type Repr = SyntaxNode;
}

impl List {
    #[allow(unused)]
    fn cast(node: &SyntaxNode) -> Option<&Self> {
        if node.kind() == LIST.into() {
            Some(Self::from_repr(node))
        } else {
            None
        }
    }

    #[allow(unused)]
    fn to_owned(&self) -> TreeArc<Self> {
        let owned = self.0.to_owned();
        TreeArc::cast(owned)
    }
}

impl Root {
    fn sexps(&self) -> impl Iterator<Item = &Sexp> {
        self.0.children().filter_map(Sexp::cast)
    }
}

impl Sexp {
    fn kind(&self) -> SexpKind {
        Atom::cast(&self.0)
            .map(SexpKind::Atom)
            .or_else(|| List::cast(&self.0).map(SexpKind::List))
            .expect("Sexp must be atom or list")
    }
}

struct Parser {
    tokens: Vec<(SyntaxKind, SmolStr)>,
    builder: GreenNodeBuilder,
    errors: Vec<String>,
}

enum SexpRes {
    Eof,
    RParen,
    Ok,
}

impl Parser {
    fn bump(&mut self) {
        let (kind, text) = self.tokens.pop().unwrap();
        self.builder.token(kind.into(), text);
    }

    fn current(&self) -> Option<SyntaxKind> {
        self.tokens.last().map(|&(kind, _)| kind)
    }

    fn skip_ws(&mut self) {
        while self.current() == Some(WHITESPACE) {
            self.bump();
        }
    }

    fn list(&mut self) {
        self.builder.start_node(LIST.into());
        self.bump(); // '('

        loop {
            match self.sexp() {
                SexpRes::Eof => {
                    self.errors.push("expected )".to_string());
                    break;
                }
                SexpRes::RParen => {
                    self.bump();
                    break;
                }
                SexpRes::Ok => {}
            }
        }

        self.builder.finish_node();
    }

    fn sexp(&mut self) -> SexpRes {
        self.skip_ws();

        let t = match self.current() {
            None => return SexpRes::Eof,
            Some(R_PAREN) => return SexpRes::RParen,
            Some(t) => t,
        };
        match t {
            L_PAREN => self.list(),
            WORD => {
                self.builder.start_node(ATOM.into());
                self.bump();
                self.builder.finish_node();
            }
            ERROR => self.bump(),
            _ => unreachable!(),
        }
        SexpRes::Ok
    }

    fn parse(mut self) -> TreeArc<Root> {
        self.builder.start_node(ROOT.into());

        loop {
            match self.sexp() {
                SexpRes::Eof => break,
                SexpRes::RParen => {
                    self.builder.start_node(ERROR.into());
                    self.errors.push("Unmatched ()".to_string());
                    self.bump();
                    self.builder.finish_node();
                }
                SexpRes::Ok => {}
            }
        }

        self.skip_ws();
        self.builder.finish_node();

        let green: GreenNode = self.builder.finish();
        let node = SyntaxNode::new(green, Some(Box::new(self.errors)));
        Root::cast(&node).unwrap().to_owned()
    }
}

fn lex(text: &str) -> Vec<(SyntaxKind, SmolStr)> {
    fn tok(t: SyntaxKind) -> m_lexer::TokenKind {
        m_lexer::TokenKind(t.0)
    }
    fn kind(t: m_lexer::TokenKind) -> SyntaxKind {
        match t.0 {
            0 => L_PAREN,
            1 => R_PAREN,
            2 => WORD,
            3 => WHITESPACE,
            4 => ERROR,
            _ => unreachable!(),
        }
    }

    let lexer = m_lexer::LexerBuilder::new()
        .error_token(tok(ERROR))
        .tokens(&[
            (tok(L_PAREN), r"\("),
            (tok(R_PAREN), r"\)"),
            (tok(WORD), r"[^\s()]+"),
            (tok(WHITESPACE), r"\s+"),
        ])
        .build();

    lexer
        .tokenize(text)
        .into_iter()
        .map(|t| (t.len, kind(t.kind)))
        .scan(0usize, |start_offset, (len, kind)| {
            let s: SmolStr = text[*start_offset..*start_offset + len].into();
            *start_offset += len;
            Some((kind, s))
        })
        .collect()
}

fn parse(text: &str) -> TreeArc<Root> {
    let mut tokens = lex(text);
    tokens.reverse();
    Parser {
        tokens: tokens,
        builder: GreenNodeBuilder::new(),
        errors: vec![],
    }
    .parse()
}

fn main() {
    let text = "
92
(+ 62 30)
(/ 92 0)
nan
(+ (* 15 2) 62)
";
    let root = parse(text);
    eprintln!("{}", root.0);
    // let res = root.sexps().map(|it| it.eval()).collect::<Vec<_>>();
    // eprintln!("{:?}", res);
    // assert_eq!(res, vec![Some(92), Some(92), None, None, Some(92),])
}
