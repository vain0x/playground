use super::*;

impl Token {
    pub(crate) fn kind(&self) -> TokenKind {
        self.kind
    }

    pub(crate) fn len(&self) -> usize {
        self.len
    }
}
