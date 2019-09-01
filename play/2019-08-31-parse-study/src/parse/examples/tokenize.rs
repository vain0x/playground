use super::*;

struct Tokenizer<'a> {
    text: &'a str,
    last: usize,
    current: usize,
}

impl<'a> Tokenizer<'a> {
    fn new(text: &'a str) -> Self {
        Tokenizer {
            text,
            last: 0,
            current: 0,
        }
    }

    fn current_str(&self) -> &str {
        &self.text[self.last..self.current]
    }

    fn next_str(&self) -> &str {
        &self.text[self.current..]
    }

    fn next_char(&self) -> char {
        self.next_str().chars().next().unwrap_or('\0')
    }

    fn bump(&mut self) {
        self.current += self.next_char().len_utf8();
    }

    fn skip(&mut self, len: usize) {
        self.current += len;
    }

    fn commit(&mut self, kind: Token) -> Token {
        self.last = self.current;
        kind
    }
}

pub(crate) fn tokenize(text: &str) -> Vec<Token> {
    let mut t = Tokenizer::new(text);
    let mut tokens = vec![];

    static TABLE: &[(&str, Token)] = &[
        ("(", Token::ParenL),
        (")", Token::ParenR),
        ("=", Token::Eq),
        ("+", Token::Plus),
        (";", Token::Semi),
        ("*", Token::Star),
    ];

    loop {
        let c = t.next_char();
        if c == '\0' {
            break;
        }

        if c.is_whitespace() {
            while t.next_char().is_whitespace() {
                t.bump();
            }
            t.commit(Token::Eof);
            continue;
        }

        if c.is_ascii_digit() {
            while t.next_char().is_ascii_digit() {
                t.bump();
            }
            tokens.push(t.commit(Token::Int));
            continue;
        }

        if c.is_alphabetic() {
            while t.next_char().is_alphanumeric() {
                t.bump();
            }
            let kind = if t.current_str() == "print" {
                Token::Print
            } else {
                Token::Ident
            };
            tokens.push(t.commit(kind));
            continue;
        }

        match TABLE
            .iter()
            .filter(|&(word, _)| t.next_str().starts_with(word))
            .next()
        {
            Some(&(word, kind)) => {
                t.skip(word.len());
                tokens.push(t.commit(kind));
                continue;
            }
            None => {}
        }

        panic!("invalid char {:?}", c)
    }

    tokens.push(Token::Eof);
    tokens
}
