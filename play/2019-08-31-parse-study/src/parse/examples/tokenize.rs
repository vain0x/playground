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

    fn commit(&mut self, kind: Token) -> TokenData {
        let span = (self.last, self.current);

        self.last = self.current;
        TokenData::new(kind, span)
    }
}

pub(crate) fn tokenize(text: &str) -> Vec<TokenData> {
    let mut t = Tokenizer::new(text);
    let mut tokens = vec![];

    static TABLE: &[(&str, Token)] = &[
        ("(", Token::ParenL),
        (")", Token::ParenR),
        ("[", Token::BracketL),
        ("]", Token::BracketR),
        ("{", Token::BraceL),
        ("}", Token::BraceR),
        (":", Token::Colon),
        (",", Token::Comma),
        ("=", Token::Eq),
        (">", Token::Gt),
        ("-", Token::Hyphen),
        ("<", Token::Lt),
        ("+", Token::Plus),
        (";", Token::Semi),
        ("*", Token::Star),
    ];

    static KEYWORDS: &[(&str, Token)] = &[
        ("print", Token::Print),
        ("if", Token::If),
        ("else", Token::Else),
        ("while", Token::While),
        ("for", Token::For),
        ("in", Token::In),
        ("fn", Token::Fn),
        ("type", Token::Type),
        ("pub", Token::Pub),
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
            // NOTE: 空白はトークン列に含めない。
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
            let kind = KEYWORDS
                .iter()
                .filter_map(|&(word, kind)| {
                    if t.current_str() == word {
                        Some(kind)
                    } else {
                        None
                    }
                })
                .next()
                .unwrap_or(Token::Ident);
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

    tokens.push(t.commit(Token::Eof));
    tokens
}
