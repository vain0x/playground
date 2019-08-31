use std::io::{self, Write};

#[derive(Clone, Debug)]
pub enum Term {
    /// 空記号
    Null,
    /// 記号
    Char(char),
    /// 連接
    Conj(Box<Term>, Box<Term>),
    /// 選択
    Disj(Box<Term>, Box<Term>),
    /// 反復
    Many(Box<Term>),
}

impl Term {
    pub fn null() -> Term {
        Term::Null
    }

    pub fn many(self) -> Term {
        Term::Many(Box::new(self))
    }

    pub fn many1(self) -> Term {
        self.clone().conj(self.clone().many())
    }

    pub fn opt(self) -> Term {
        self.disj(Term::Null)
    }

    pub fn conj(self, second: Term) -> Term {
        Term::Conj(Box::new(self), Box::new(second))
    }

    pub fn disj(self, other: Term) -> Term {
        Term::Disj(Box::new(self), Box::new(other))
    }

    pub fn parse(text: &str) -> Term {
        let mut i = 0;
        let t = parse::p_term(text, &mut i);
        assert_eq!(i, text.len());
        t
    }

    pub fn show(&self) -> String {
        fn write(term: &Term, out: &mut Vec<u8>) -> io::Result<()> {
            match term {
                Term::Null => write!(out, "ε"),
                Term::Char(c) => write!(out, "'{}'", c),
                Term::Many(t) => match **t {
                    Term::Null | Term::Char(..) | Term::Many(..) => {
                        write(t, out)?;
                        write!(out, "*")
                    }
                    _ => {
                        write!(out, "(")?;
                        write(t, out)?;
                        write!(out, ")*")
                    }
                },
                Term::Conj(l, r) => {
                    write(l, out)?;
                    write!(out, " ")?;
                    write(r, out)
                }
                Term::Disj(l, r) => {
                    write(l, out)?;
                    write!(out, "|")?;
                    write(r, out)
                }
            }
        }

        let mut out = vec![];
        write(self, &mut out).unwrap();
        String::from_utf8(out).unwrap()
    }
}

impl From<char> for Term {
    fn from(c: char) -> Term {
        Term::Char(c)
    }
}

impl From<&str> for Term {
    fn from(s: &str) -> Term {
        s.chars().fold(Term::Null, |t, c| t.disj(Term::Char(c)))
    }
}

mod parse {
    use super::*;

    fn char_is_meta(c: char) -> bool {
        ")*+?|".contains(c)
    }

    fn p_atom(text: &str, i: &mut usize) -> Term {
        match text[*i..].chars().next() {
            Some('(') => {
                *i += 1;
                let term = p_term(text, i);
                eprintln!("{} {}", *i, &text[*i..]);
                assert!(text[*i..].starts_with(")"), "Expected ) at {}", *i);
                *i += 1;
                term
            }
            Some('[') => {
                *i += 1;
                let start = *i;
                let end = match text[*i..].find(']') {
                    None => panic!("Expected ] but EOF"),
                    Some(len) => {
                        *i += len + 1;
                        start + len
                    }
                };
                Term::from(&text[start..end])
            }
            Some(c) if !char_is_meta(c) => {
                *i += c.len_utf8();
                Term::from(c)
            }
            _ => Term::null(),
        }
    }

    fn p_suffix(text: &str, i: &mut usize) -> Term {
        let mut t = p_atom(text, i);

        loop {
            match text[*i..].chars().next() {
                Some('*') => {
                    *i += 1;
                    t = t.many();
                }
                Some('+') => {
                    *i += 1;
                    t = t.many1();
                }
                Some('?') => {
                    *i += 1;
                    t = t.opt();
                }
                _ => break,
            }
        }
        t
    }

    fn p_conj(text: &str, i: &mut usize) -> Term {
        let mut t = p_suffix(text, i);

        loop {
            match text[*i..].chars().next() {
                Some(c) if !char_is_meta(c) => {
                    let second = p_suffix(text, i);
                    t = t.conj(second);
                }
                _ => break,
            }
        }

        t
    }

    fn p_disj(text: &str, i: &mut usize) -> Term {
        let mut t = p_conj(text, i);

        while let Some('|') = text[*i..].chars().next() {
            *i += 1;
            let second = p_conj(text, i);
            t = t.disj(second);
        }

        t
    }

    pub(super) fn p_term(text: &str, i: &mut usize) -> Term {
        if text[*i..].is_empty() {
            Term::null()
        } else {
            p_disj(text, i)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        fn t(input: &str, expected: &str) {
            assert_eq!(&Term::parse(input).show(), expected);
        }

        t("1+", "'1' '1'*");
        t("[01]*", "(ε|'0'|'1')*");
        t("(0|1)(2|3)", "'0'|'1' '2'|'3'");
    }
}
