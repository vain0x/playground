use super::*;

pub struct Lexer {
    source: String,
    prev: usize,
    current: usize,
    tokens: Vec<Token>,
}

impl Lexer {
    fn source(&self) -> &str {
        &self.source
    }

    fn at_eof(&self) -> bool {
        self.current >= self.source().len()
    }

    fn next_char(&self) -> u8 {
        if self.at_eof() {
            return 0;
        }
        self.source().as_bytes()[self.current]
    }

    fn next_str(&self) -> &str {
        &self.source[self.current..]
    }

    fn add_token(&mut self, kind: SyntaxKind) {
        let len = self.current - self.prev;
        self.tokens.push(Token { kind, len });
        self.prev = self.current;
    }

    fn read_while(&mut self, pred: impl Fn(u8) -> bool) -> bool {
        if !pred(self.next_char()) {
            return false;
        }
        while !self.at_eof() && pred(self.next_char()) {
            self.current += 1;
        }
        true
    }

    fn reads(&mut self, prefix: &str) -> bool {
        if self.next_str().starts_with(prefix) {
            self.current += prefix.len();
            return true;
        }
        false
    }

    fn read_any_char(&mut self) {
        let char_len = first_char_len(self.next_str());
        self.current += char_len;
    }

    pub fn tokenize(&mut self) {
        while !self.at_eof() {
            if self.read_while(is_whitespace) {
                self.add_token(SyntaxKind::WhitespaceToken);
                continue;
            }
            if self.read_while(is_ascii_digit) {
                self.add_token(SyntaxKind::IntToken);
                continue;
            }
            if self.reads("+") {
                self.add_token(SyntaxKind::PlusToken);
                continue;
            }
            if self.reads("-") {
                self.add_token(SyntaxKind::MinusToken);
                continue;
            }

            self.read_any_char();
            self.add_token(SyntaxKind::Error);
        }
        self.add_token(SyntaxKind::EofToken);
    }
}

pub(crate) fn tokenize(source: String) -> Vec<Token> {
    let mut tokenizer = Lexer {
        source,
        prev: 0,
        current: 0,
        tokens: vec![],
    };
    tokenizer.tokenize();
    tokenizer.tokens
}

fn first_char_len(text: &str) -> usize {
    text.chars()
        .next()
        .expect("Any character is expected")
        .len_utf8()
}

pub(crate) fn is_whitespace(c: u8) -> bool {
    c == b' ' || c == b'\t' || c == b'\r' || c == b'\n'
}

pub(crate) fn is_ascii_digit(c: u8) -> bool {
    b'0' <= c && c <= b'9'
}
