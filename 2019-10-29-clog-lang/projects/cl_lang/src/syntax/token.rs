use super::*;
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Error,
    Eof,
    Space,
    Ident,
    Atom,
    ParenL,
    ParenR,
    BraceL,
    BraceR,
    Semi,
    False,
    True,
    Rule,
}

#[derive(Clone, Debug)]
pub struct Token {
    kind: TokenKind,
    text: RcStr,
}

impl Token {
    pub(crate) fn new(kind: TokenKind, text: RcStr) -> Self {
        Self { kind, text }
    }

    pub(crate) fn new_miss() -> Self {
        let text = RcStr::new(Rc::new(String::new()), (0, 0));
        Token::new(TokenKind::Error, text)
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn text(&self) -> &str {
        self.text.as_str()
    }

    pub fn text_len(&self) -> usize {
        self.text().len()
    }
}
