use super::*;
use std::rc::Rc;

pub(crate) struct TokenStream {
    text: Rc<String>,
    last: usize,
    current: usize,
}

const PUNCTUATIONS: &'static [(&'static str, TokenKind)] = &[
    ("(", TokenKind::ParenL),
    (")", TokenKind::ParenR),
    ("{", TokenKind::BraceL),
    ("}", TokenKind::BraceR),
    (";", TokenKind::Semi),
];

const KEYWORDS: &'static [(&'static str, TokenKind)] =
    &[("rule", TokenKind::Rule)];

fn char_is_ident(c: char) -> bool {
    c == '_' || (!c.is_whitespace() && !c.is_ascii_punctuation() && !c.is_control() && c != '\0')
}

fn char_is_atom_first(c: char) -> bool {
    char_is_ident(c) && c != '_' && !c.is_lowercase()
}

impl TokenStream {
    pub(crate) fn new(text: Rc<String>) -> Self {
        Self {
            text: Rc::clone(&text),
            last: 0,
            current: 0,
        }
    }

    pub(crate) fn current_str(&self) -> &str {
        &self.text[self.last..self.current]
    }

    pub(crate) fn next_char(&self) -> char {
        self.text[self.current..].chars().next().unwrap_or('\0')
    }

    pub(crate) fn next_str(&self) -> &str {
        &self.text[self.current..]
    }

    pub(crate) fn at(&self, pred: impl Fn(char) -> bool) -> bool {
        pred(self.next_char())
    }

    fn advance(&mut self, len: usize) {
        debug_assert!(self.current + len <= self.text.len());
        self.current += len;
    }

    pub(crate) fn eat(&mut self, text: &str) -> bool {
        if self.next_str().starts_with(text) {
            self.advance(text.len());
            return true;
        }

        false
    }

    pub(crate) fn eat_while(&mut self, pred: impl Fn(char) -> bool) {
        while self.next_char() != '\0' && pred(self.next_char()) {
            self.advance(self.next_char().len_utf8());
        }
    }

    pub(crate) fn emit(&mut self, kind: TokenKind) -> Token {
        let text = RcStr::new(Rc::clone(&self.text), (self.last, self.current));
        let token = Token::new(kind, text);
        self.last = self.current;
        token
    }

    pub(crate) fn bump(&mut self) -> Token {
        if self.at(|c| c.is_whitespace()) {
            self.eat_while(|c| c.is_whitespace());
            return self.emit(TokenKind::Space);
        }

        for &(pun, kind) in PUNCTUATIONS {
            if self.eat(pun) {
                return self.emit(kind);
            }
        }

        if self.at(char_is_atom_first) {
            self.eat_while(char_is_ident);
            return self.emit(TokenKind::Atom);
        }

        if self.at(char_is_ident) {
            self.eat_while(char_is_ident);
            let text = self.current_str();
            let kind = KEYWORDS
                .iter()
                .filter_map(|&(t, k)| if t == text { Some(k) } else { None })
                .next()
                .unwrap_or(TokenKind::Ident);
            return self.emit(kind);
        }

        if self.at(|c| c == '\0') {
            return self.emit(TokenKind::Eof);
        }

        self.advance(self.next_char().len_utf8());
        self.emit(TokenKind::Error)
    }
}
