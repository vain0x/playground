use crate::coord::*;
use std::{collections::VecDeque, fmt::Debug};

#[derive(Debug)]
enum Formula {
    Number(String),
    Ref(GridVec),
    Range(GridRange),
}

fn parse_ref(s: &str) -> Option<GridVec> {
    if !(2 <= s.len() && s.as_bytes()[0].is_ascii_uppercase()) {
        return None;
    }

    let column = (s.as_bytes()[0] - b'A') as u32;
    debug_assert!(column < 26);

    let row = s[1..].parse::<u32>().ok()?;
    debug_assert!(row < 100);

    Some(GridVec::new(row, column))
}

#[derive(Debug)]
enum Token {
    Blank,
    Number(String),
    Ref(GridVec),
    Ident(String),
    LeftParen,
    RightParen,
    Colon,
    Comma,
}

fn tokenize_next(s: &str, i: usize) -> Option<(Token, usize)> {
    match s.as_bytes()[i] {
        b' ' | b'\t' | b'\r' | b'\n' => Some((Token::Blank, 1)),
        b'(' => Some((Token::LeftParen, 1)),
        b')' => Some((Token::RightParen, 1)),
        b':' => Some((Token::Colon, 1)),
        b',' => Some((Token::Comma, 1)),

        c if c == b'-' || c == b'.' || c.is_ascii_digit() => {
            let mut r = i + 1;
            while r < s.len() && {
                let c = s.as_bytes()[r];
                c.is_ascii_digit() || c == b'.'
            } {
                r += 1;
            }

            let text = &s[i..r];

            let ok = text.parse::<f64>().is_ok()
                && !(r < s.len() && s.as_bytes()[r].is_ascii_alphabetic());
            if !ok {
                return None;
            }

            Some((Token::Number(text.into()), r - i))
        }

        c if c.is_ascii_alphabetic() => {
            let mut r = i + 1;
            while r < s.len() && s.as_bytes()[r].is_ascii_alphanumeric() {
                r += 1;
            }
            let text = &s[i..r];
            let token = match parse_ref(text) {
                Some(v) => Token::Ref(v),
                None => Token::Ident(text.into()),
            };
            Some((token, r - i))
        }

        _ => None,
    }
}

fn tokenize(s: &str) -> Option<Vec<Token>> {
    let mut tokens = vec![];
    let mut i = 0;

    while i < s.len() {
        let (token, len) = tokenize_next(s, i)?;
        i += len;

        match token {
            Token::Blank => {}
            _ => tokens.push(token),
        }
    }

    Some(tokens)
}

fn parse_expr(tokens: &mut VecDeque<Token>) -> Option<Formula> {
    match tokens.pop_front()? {
        Token::Number(value) => Some(Formula::Number(value)),
        Token::Ident(_) => todo!("function application"),
        Token::Ref(s) => match tokens.get(0) {
            Some(Token::Colon) => {
                tokens.pop_front();
                let t = match tokens.pop_front() {
                    Some(Token::Ref(t)) => t,
                    _ => return None,
                };
                Some(Formula::Range(GridRange::new(s, t)))
            }
            _ => Some(Formula::Ref(s)),
        },
        Token::LeftParen => {
            let body = parse_expr(tokens)?;
            match tokens.pop_front() {
                Some(Token::RightParen) => {}
                _ => return None,
            }
            Some(body)
        }
        Token::Blank => unreachable!(),
        Token::RightParen | Token::Colon | Token::Comma => None,
    }
}

#[allow(unused)]
fn parse_formula(s: &str) -> Option<Formula> {
    let tokens = tokenize(s)?;

    let mut tokens = VecDeque::from(tokens);
    let f = parse_expr(&mut tokens)?;

    if !tokens.is_empty() {
        return None;
    }

    Some(f)
}

#[cfg(test)]
mod tests {
    use super::parse_formula;

    #[test]
    fn test_parse() {
        fn p(s: &str) -> String {
            match parse_formula(s) {
                Some(f) => format!("{f:?}"),
                None => "None".to_string(),
            }
        }

        assert_eq!(p("0"), r#"Number("0")"#);
        assert_eq!(p("-1"), r#"Number("-1")"#);
        assert_eq!(p("01.10"), r#"Number("01.10")"#);
        assert_eq!(p("42.159"), r#"Number("42.159")"#);

        assert_eq!(p("A0"), "Ref((0, 0))");
        assert_eq!(p("A1:B2"), "Range((1, 0)-(2, 1))");

        assert_eq!(p("( 42.0 )"), r#"Number("42.0")"#);

        assert_eq!(p(""), "None");
        assert_eq!(p("0 1"), "None");
        assert_eq!(p("0x7f"), "None");
        assert_eq!(p("A0:"), "None");
        assert_eq!(p("A0::"), "None");
        assert_eq!(p(":A0"), "None");
    }
}
