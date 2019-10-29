use super::*;
use std::collections::VecDeque;
use std::rc::Rc;

pub struct Parser {
    file: SourceFile,
    nexts: VecDeque<(Token, TextPos)>,
    tokens: TokenStream,
    cursor: TextPosCursor,
}

impl Parser {
    pub(crate) fn new(file: SourceFile) -> Self {
        let text = file.text();
        Parser {
            file,
            nexts: VecDeque::new(),
            tokens: TokenStream::new(Rc::clone(&text)),
            cursor: TextPosCursor::new(text),
        }
    }

    fn loc(&self, pos: TextPos) -> SourceLocation {
        self.file.loc(pos)
    }

    fn look_ahead(&mut self, count: usize) {
        while self.nexts.len() < count {
            let pos = self.cursor.pos();
            let token = self.tokens.bump();
            self.cursor.advance(token.text_len());

            if token.kind() == TokenKind::Space {
                continue;
            }

            if token.kind() == TokenKind::Error {
                eprintln!("WARN: Skipping invalid token at {:?}", self.loc(pos));
                continue;
            }

            self.nexts.push_back((token, pos));
        }

        assert!(self.nexts.len() >= count);
    }

    pub(crate) fn next_kind(&mut self) -> TokenKind {
        self.look_ahead(1);

        self.nexts[0].0.kind()
    }

    pub(crate) fn at(&mut self, kind: TokenKind) -> bool {
        self.next_kind() == kind
    }

    pub(crate) fn bump(&mut self) -> Token {
        self.look_ahead(1);

        self.nexts.pop_front().unwrap().0
    }

    pub(crate) fn error(&mut self, msg: &str) {
        self.look_ahead(1);

        let pos = self.nexts[0].1;
        eprintln!("ERROR: {} ({:?})", msg, self.loc(pos));
    }
}
