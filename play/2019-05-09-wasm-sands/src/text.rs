pub(crate) mod tokenize;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum TokenKind {
    /// Invalid character.
    Error,
    Whitespace,
    /// Integer.
    Int,
    /// Double-quoted.
    Str,
    /// Identifier, e.g. `$foo`.
    Ident,
    /// E.g. `i32`.
    Keyword,
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
