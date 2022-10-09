use crate::coord::*;
use std::{collections::VecDeque, fmt::Debug};

#[derive(Clone, Debug)]
pub(crate) enum Fn {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Sum,
    Prod,
}

impl Fn {
    fn parse(s: &str) -> Option<Fn> {
        let it = match s {
            "add" => Fn::Add,
            "sub" => Fn::Subtract,
            "mul" => Fn::Multiply,
            "div" => Fn::Divide,
            "mod" => Fn::Modulo,
            "sum" => Fn::Sum,
            "prod" => Fn::Prod,
            _ => {
                eprintln!("unknown fn '{}'", s);
                return None;
            }
        };
        Some(it)
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Formula {
    Number(String),
    Call(Fn, Vec<Formula>),
    Ref(GridVec),
    Range(GridRange),
}

impl Formula {
    #[allow(unused)]
    pub(crate) fn parse(s: &str) -> Option<Formula> {
        parse_formula(s)
    }
}

/// Parse cell-reference notation, e.g. `A1`.
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
        Token::Ident(name) => {
            let fn_kind = Fn::parse(&name)?;

            match tokens.pop_front() {
                Some(Token::LeftParen) => {}
                _ => return None,
            }

            let mut args = vec![];

            match tokens.get(0) {
                Some(Token::RightParen) => {}
                _ => loop {
                    let arg = parse_expr(tokens)?;
                    args.push(arg);

                    match tokens.get(0) {
                        Some(Token::RightParen) => break,
                        Some(Token::Comma) => {
                            tokens.pop_front();
                            continue;
                        }
                        _ => return None,
                    }
                },
            }

            match tokens.pop_front() {
                Some(Token::RightParen) => {}
                _ => return None,
            }

            Some(Formula::Call(fn_kind, args))
        }
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
    use super::Formula;

    #[test]
    fn test_parse() {
        fn p(s: &str) -> String {
            match Formula::parse(s) {
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

        assert_eq!(p("add(A1, 1)"), r#"Call(Add, [Ref((1, 0)), Number("1")])"#);
        assert_eq!(
            p("div(sum(A1:A2), 2)"),
            r#"Call(Divide, [Call(Sum, [Range((1, 0)-(2, 0))]), Number("2")])"#
        );
        assert_eq!(p("add("), "None");
        assert_eq!(p("add )"), "None");
        assert_eq!(p("(add)"), "None");
        assert_eq!(p("(add)()"), "None");
        assert_eq!(p("unknown(1, 2))"), "None");
    }
}
