pub(crate) mod assemble;
pub(crate) mod ast;
pub(crate) mod grammar;
pub(crate) mod keyword;
pub(crate) mod parser;
pub(crate) mod syn;
pub(crate) mod token;
pub(crate) mod tokenize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct StrId(usize);

impl StrId {
    pub(crate) fn new(str_id: usize) -> StrId {
        StrId(str_id)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Keyword {
    Unknown,
    Module,
    Func,
    Result,
    I32,
    Const,
    Add,
    Export,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum TokenKind {
    /// Invalid character.
    Error,
    Eof,
    Whitespace,
    /// Integer.
    Int,
    /// Double-quoted.
    Str,
    /// Identifier, e.g. `$foo`.
    Ident,
    /// E.g. `i32`.
    Keyword(Keyword),
    Dot,
    /// '('
    ParenL,
    /// ')'
    ParenR,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Token {
    kind: TokenKind,
    /// In bytes.
    len: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum SynKind {
    Error,
    Token(TokenKind),
    Root,
    ModuleDecl,
    FuncDecl,
    ResultTy,
    Op,
    Instr,
    Val,
    ExportDecl,
    ExportDesc,
}

pub(crate) type SynId = usize;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Syn {
    Token(Token),
    Node {
        kind: SynKind,
        children: Vec<SynId>,
        len: usize,
    },
}

pub(crate) struct SynTree<'a> {
    text: &'a str,
    nodes: Vec<Syn>,
    errors: Vec<((usize, usize), String)>,
}

pub(crate) trait Ast: Sized {
    fn syn_id(&self) -> SynId;

    fn cast(syn_id: SynId, st: &SynTree<'_>) -> Option<Self>;

    fn as_syn<'a>(&self, st: &'a SynTree<'a>) -> &'a Syn {
        &st.nodes[self.syn_id()]
    }
}
