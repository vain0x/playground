use super::*;
use std::fmt::{self, Debug, Formatter};
use std::io::{self, Write};

type Span = (usize, usize);

#[derive(Clone, Debug)]
pub(crate) struct Node {
    kind: Symbol,
    span: Span,
    children: Vec<Node>,
}

impl Node {
    pub(crate) fn new(kind: Symbol, span: Span, children: Vec<Node>) -> Self {
        Self {
            kind,
            span,
            children,
        }
    }

    pub(crate) fn is_token(&self) -> bool {
        self.kind.as_token().is_some()
    }

    fn write(&self, text: &str, indent: usize, out: &mut Vec<u8>) -> io::Result<()> {
        match self.kind {
            Symbol::Token(token) => {
                let (l, r) = self.span;
                write!(out, "{:?}({:?})", token, &text[l..r])
            }
            Symbol::NonTerm(non_term) => {
                write!(out, "{:?}:", non_term)?;

                for child in self.children.iter() {
                    write!(out, "\n")?;
                    for _ in 0..(indent + 1) * 4 {
                        write!(out, " ")?;
                    }
                    child.write(text, indent + 1, out)?;
                }

                Ok(())
            }
        }
    }
}

impl From<TokenData> for Node {
    fn from(token: TokenData) -> Self {
        Self::new(Symbol::from(token.kind()), token.span(), vec![])
    }
}

#[derive(Clone)]
pub(crate) struct ParseTree {
    text: String,
    root: Node,
}

impl ParseTree {
    pub(crate) fn new(text: String, root: Node) -> Self {
        Self { text, root }
    }

    pub(crate) fn from_events(
        text: String,
        tokens: Vec<TokenData>,
        events: impl IntoIterator<Item = Event>,
    ) -> Self {
        let mut stack = vec![];
        let mut token_iter = tokens.into_iter();

        for event in events {
            match event {
                Event::Shift => {
                    let token = token_iter.next().unwrap();
                    stack.push(Node::from(token));
                }
                Event::Reduce(non_term, count) => {
                    // count == 1 は Int → atom → ... → expr のように無駄に長く、AST に残らないでなるべく省略する。
                    // ただしトークンから非終端記号への直接の変換は AST に影響するので省略しない。
                    let omit = count == 1 && stack.last().map(|n| !n.is_token()).unwrap();

                    if !omit {
                        let mut children = vec![];
                        for _ in 0..count {
                            let node = stack.pop().unwrap();
                            children.push(node);
                        }
                        children.reverse();
                        stack.push(Node::new(Symbol::from(non_term), (0, 0), children));
                    }
                }
            }
        }

        let node = stack.pop().unwrap();
        assert!(stack.is_empty());

        // ルートは還元されない (先に受理される) のでここで挿入する。
        let root = Node::new(Symbol::from(NonTerm::new("root")), (0, 0), vec![node]);

        Self::new(text, root)
    }
}

impl Debug for ParseTree {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let mut out = vec![];
        self.root.write(&self.text, 0, &mut out).unwrap();
        let out = String::from_utf8(out).unwrap();
        f.write_str(&out)
    }
}
