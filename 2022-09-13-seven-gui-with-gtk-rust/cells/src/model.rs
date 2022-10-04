use std::{collections::VecDeque, fmt::Debug};

/// グリッド上のベクトル
#[derive(Clone, Copy, Default, PartialEq, PartialOrd)]
pub(crate) struct GridVec {
    pub(crate) y: u32,
    pub(crate) x: u32,
}

impl GridVec {
    pub(crate) fn new(y: u32, x: u32) -> Self {
        GridVec { y, x }
    }
}

impl std::ops::Add for GridVec {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        GridVec {
            y: self.y + rhs.y,
            x: self.x + rhs.x,
        }
    }
}

impl std::ops::Sub for GridVec {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        GridVec {
            y: self.y - rhs.y,
            x: self.x - rhs.x,
        }
    }
}

impl Debug for GridVec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.y, self.x)
    }
}

/// グリッド上の範囲
#[derive(Clone, Copy, Default)]
pub(crate) struct GridRange {
    pub(crate) s: GridVec,
    pub(crate) t: GridVec,
}

impl GridRange {
    pub(crate) fn new(s: GridVec, t: GridVec) -> Self {
        GridRange { s, t }
    }
}

impl Debug for GridRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}-{:?}", self.s, self.t)
    }
}

#[derive(Debug)]
enum Formula {
    Number(String),
    Ref(GridVec),
    Range(GridRange),
}

fn parse_ref(s: &str) -> Option<GridVec> {
    if !(2 <= s.len() && s.len() <= 3) {
        return None;
    }
    if !s.as_bytes()[0].is_ascii_uppercase() {
        return None;
    }

    let column = (s.as_bytes()[0] - b'A') as u32;
    debug_assert!(column < 26);

    let row = s[1..].parse::<u32>().ok()?;
    debug_assert!(row < 100);

    Some(GridVec::new(row, column))
}

#[allow(unused)]
fn parse_formula(s: &str) -> Option<Formula> {
    #[derive(Debug)]
    enum Token {
        Number(String),
        Ref(GridVec),
        Ident(String),
        LeftParen,
        RightParen,
        Colon,
        Comma,
    }

    let mut i = 0;
    let mut tokens = vec![];

    while i < s.len() {
        match s.as_bytes()[i] {
            b' ' | b'\t' | b'\r' | b'\n' => {
                i += 1;
                continue;
            }
            b'(' => {
                tokens.push(Token::LeftParen);
                i += 1
            }
            b')' => {
                tokens.push(Token::RightParen);
                i += 1
            }
            b':' => {
                tokens.push(Token::Colon);
                i += 1
            }
            b',' => {
                tokens.push(Token::Comma);
                i += 1
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
                tokens.push(token);
                i = r;
            }
            c if c == b'-' || c.is_ascii_digit() => {
                let mut r = i + 1;
                while r < s.len() && {
                    let c = s.as_bytes()[r];
                    c.is_ascii_digit() || c == b'.'
                } {
                    r += 1;
                }

                let text = &s[i..r];
                if text.parse::<f64>().is_err() {
                    eprintln!("cannot parse number {:?}", text);
                    return None;
                }

                tokens.push(Token::Number(text.into()));
                i = r;

                if r < s.len() && s.as_bytes()[r].is_ascii_alphabetic() {
                    eprintln!("invalid char {:?} at {i}", &s[r..r + 1]);
                    return None;
                }
            }
            _ => {
                eprintln!("invalid char {:?} at {i}", s.as_bytes()[i] as char);
                return None;
            }
        }
    }

    fn parse_expr(tokens: &mut VecDeque<Token>) -> Option<Formula> {
        let f = match tokens.pop_front()? {
            Token::Number(value) => Formula::Number(value),
            Token::Ident(_) => todo!("function application"),
            Token::Ref(s) => match tokens.get(0) {
                Some(Token::Colon) => {
                    tokens.pop_front();
                    let t = match tokens.pop_front() {
                        Some(Token::Ref(t)) => t,
                        _ => {
                            eprintln!("range syntax error");
                            return None;
                        }
                    };
                    Formula::Range(GridRange::new(s, t))
                }
                _ => Formula::Ref(s),
            },
            Token::LeftParen => {
                let body = parse_expr(tokens)?;

                match tokens.pop_front() {
                    Some(Token::RightParen) => {}
                    _ => {
                        eprintln!("expected right paren");
                        return None;
                    }
                }
                body
            }
            t @ (Token::RightParen | Token::Colon | Token::Comma) => {
                eprintln!("unexpected token {:?}", t);
                return None;
            }
        };
        Some(f)
    }

    let mut tokens = VecDeque::from(tokens);
    let f = parse_expr(&mut tokens)?;

    if !tokens.is_empty() {
        eprintln!("unexpected token {:?}", &tokens[0]);
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
                _ => "None".into(),
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
