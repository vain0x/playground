use super::*;
use std::cell::Cell;
use std::cmp::min;

struct Tokenize<'a> {
    text: &'a str,

    /// The end index of the last token or initially 0.
    last: usize,

    /// Current position.
    /// `last..index` is the span for token created next.
    index: usize,

    tokens: Vec<Token>,

    /// Count how many times it is working to detect infinite loop.
    tick: Cell<usize>,
}

impl<'a> Tokenize<'a> {
    fn new(text: &'a str) -> Self {
        Tokenize {
            text,
            last: 0,
            index: 0,
            tokens: vec![],
            tick: Cell::new(0),
        }
    }

    fn detect_infinite_loop(&self) {
        let tick = self.tick.get();
        self.tick.set(tick + 1);

        if tick >= 10_000_000 {
            panic!("Infinite loop?");
        }
    }

    fn assert_invariant(&self) {
        assert!(self.last <= self.index);
        assert!(self.index <= self.text.len());
    }

    fn next(&self) -> u8 {
        self.detect_infinite_loop();
        self.assert_invariant();

        if self.index >= self.text.len() {
            return 0;
        }

        self.text.as_bytes()[self.index]
    }

    fn advance(&mut self, len: usize) {
        self.index = min(self.index + len, self.text.len());
    }

    fn bump(&mut self) {
        self.advance(1);
    }

    fn eat(&mut self, word: &str) -> bool {
        if self.text[self.index..].starts_with(word) {
            self.index += word.len();
            true
        } else {
            false
        }
    }

    fn current_text(&self) -> &str {
        &self.text[self.last..self.index]
    }

    /// Add a token with the specified kind.
    /// It spans over the range from the last position where previously `complete` is called
    /// to the current position.
    fn complete(&mut self, kind: TokenKind) {
        let len = self.index - self.last;

        self.tokens.push(Token { kind, len });
        self.last = self.index;
    }

    fn read_error_char(&mut self) {
        if let Some(c) = self.text[self.index..].chars().next() {
            self.index += c.len_utf8();
            self.complete(TokenKind::Error);
        }
    }

    fn read_whitespace(&mut self) {
        if self.next().is_ascii_whitespace() {
            while self.next().is_ascii_whitespace() {
                self.bump();
            }
            self.complete(TokenKind::Whitespace);
        }
    }

    fn read_int(&mut self) {
        if self.next().is_ascii_digit() {
            while self.next().is_ascii_digit() {
                self.bump();
            }
            self.complete(TokenKind::Int);
        }
    }

    fn read_str(&mut self) {
        if self.next() == b'"' {
            self.bump();

            // FIXME: escape sequence

            while self.next() != b'"' && self.next() != b'\n' && self.next() != 0 {
                self.bump();
            }

            if self.next() == b'"' {
                self.bump();
            }

            self.complete(TokenKind::Str);
        }
    }

    fn read_ident(&mut self) {
        if self.next() == b'$' {
            self.bump();

            // FIXME: some symbols are also allowed

            while self.next().is_ascii_alphanumeric() {
                self.bump();
            }

            self.complete(TokenKind::Ident);
        }
    }

    fn read_keyword(&mut self) {
        if self.next().is_ascii_alphabetic() {
            while self.next().is_ascii_alphanumeric() {
                self.bump();
            }

            let keyword = Keyword::parse(self.current_text());
            self.complete(TokenKind::Keyword(keyword));
        }
    }

    fn read_pun(&mut self) {
        const PUN: &[(TokenKind, &str)] = &[
            (TokenKind::Dot, "."),
            (TokenKind::ParenL, "("),
            (TokenKind::ParenR, ")"),
        ];

        for &(kind, text) in PUN {
            if self.eat(text) {
                self.complete(kind);
            }
        }
    }

    fn tokenize(&mut self) {
        while self.next() != 0 {
            let start = self.index;

            self.read_whitespace();
            self.read_int();
            self.read_str();
            self.read_ident();
            self.read_keyword();
            self.read_pun();

            // The next character doesn't match any token.
            let stuck = self.next() != 0 && self.index == start;
            if stuck {
                self.read_error_char();
            }
        }

        self.complete(TokenKind::Eof);
    }
}

pub(crate) fn tokenize(text: &str) -> Vec<Token> {
    let mut t = Tokenize::new(text);
    t.tokenize();
    t.tokens
}

fn inspect<'a>(text: &'a str, tokens: &'a [Token]) -> Vec<&'a str> {
    let mut out = Vec::with_capacity(tokens.len());
    let mut r = 0;

    for token in tokens {
        let l = r;
        r += token.len;

        if token.kind == TokenKind::Whitespace || token.kind == TokenKind::Eof {
            continue;
        }

        let t = &text[l..r];
        out.push(t);
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_simple_example() {
        let text = r#"
(module
  (func $add (result i32)
    i32.const 2
    i32.const 3
    i32.add)
  (export "add" (func $add)))
        "#;

        let tokens = tokenize(text);
        let token_texts = inspect(text, &tokens);

        assert_eq!(
            token_texts,
            vec![
                "(", "module", "(", "func", "$add", "(", "result", "i32", ")", "i32", ".", "const",
                "2", "i32", ".", "const", "3", "i32", ".", "add", ")", "(", "export", "\"add\"",
                "(", "func", "$add", ")", ")", ")"
            ]
        );
    }

    #[test]
    fn test_tokenize_invalid_text() {
        let text = r#"*🐈* is emoji of 猫."#;

        let tokens = tokenize(text);
        assert!(tokens
            .into_iter()
            .any(|token| token.kind == TokenKind::Error));
    }
}
