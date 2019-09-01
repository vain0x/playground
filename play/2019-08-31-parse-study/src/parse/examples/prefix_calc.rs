//! 前置記法の四則演算の文法

use super::*;
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

    // S' -> S $
    g.add_rule(rule!(sp => [s, Eof]));

    // S -> = id E
    g.add_rule(rule!(s => [Eq, Ident, e]));

    // S -> print E
    g.add_rule(rule!(s => [Print, e]));

    // E -> + E E
    g.add_rule(rule!(e => [Plus, e, e]));

    // E -> * E E
    g.add_rule(rule!(e => [Star, e, e]));

    // E -> id
    g.add_rule(rule!(e => [Ident]));

    // E -> num
    g.add_rule(rule!(e => [Int]));

    (g, sp)
}

#[allow(unused)]
fn parse(text: &str) {
    let tokens = tokenize(text);
    let (grammar, non_term) = make_grammar();
    super::super::ll1::parse(tokens, non_term, &grammar);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_accept() {
        parse("= a 1");
        parse("= a + 1 2");
        parse("= a + 1 2");
        parse("= a + 1 * 2 3");
        parse("print * 2 + * 2 3 2");
        parse("= a + a 1");
    }

    #[test]
    #[should_panic]
    fn test_reject() {
        parse("= a");
    }

    #[test]
    #[should_panic]
    fn test_reject2() {
        parse("= print 1");
    }
}
