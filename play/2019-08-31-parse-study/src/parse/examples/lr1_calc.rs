//! LR(1) 文法の数式

use super::*;
use crate::parse::examples::tokenize::tokenize;
use Token::*;

macro_rules! rule {
    ($s:ident => [$($t:expr),*]) => {{
        let mut target = vec![];
        $(target.push($t.into());)*

        ($s).to_rule(target)
    }}
}

fn make_grammar() -> (Grammar, NonTerm) {
    let mut g = Grammar::new();

    let sp = NonTerm::new("S'");
    let s = NonTerm::new("S");
    let e = NonTerm::new("E");
    let f = NonTerm::new("F");

    // S' -> S $
    g.add_rule(rule!(sp => [s, Eof]));

    // S -> id ( E )
    g.add_rule(rule!(s => [Ident, ParenL, e, ParenR]));

    // E -> E + F
    g.add_rule(rule!(e => [e, Plus, f]));

    // E -> F
    g.add_rule(rule!(e => [f]));

    // F -> num
    g.add_rule(rule!(f => [Int]));

    // F -> ( E )
    g.add_rule(rule!(f => [ParenL, e, ParenR]));

    (g, sp)
}

#[allow(unused)]
fn parse(text: &str) -> bool {
    let tokens = tokenize(text);
    let (grammar, non_term) = make_grammar();
    crate::parse::lr1::functions::parse(tokens, non_term, grammar)
}

pub(crate) fn test() -> bool {
    parse("f(1)")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_accept() {
        assert!(parse("f(2)"));
        assert!(parse("f(2 + 3)"));

        assert!(parse("f(2 + (3 + 4) + (5))"));
    }

    #[test]
    fn test_reject() {
        assert!(!parse("f()"));
        assert!(!parse("f(a)"));
        assert!(!parse("f(+)"));
        assert!(!parse("f(2 +"));
        assert!(!parse("2 + 3"));

        assert!(!parse("f(1 + ())"));
        assert!(!parse("f(((((((((((((((("));
    }
}
