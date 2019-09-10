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

    let root = NonTerm::new("root");
    let stmt = NonTerm::new("stmt");
    let expr = NonTerm::new("expr");
    let add = NonTerm::new("add");
    let mul = NonTerm::new("mul");
    let atom = NonTerm::new("atom");

    g.add_rule(rule!(root => [stmt, Eof]));

    g.add_rule(rule!(stmt => [expr]));

    g.add_rule(rule!(expr => [add]));

    g.add_rule(rule!(add => [add, Plus, mul]));

    g.add_rule(rule!(add => [add, Hyphen, mul]));

    g.add_rule(rule!(add => [mul]));

    g.add_rule(rule!(mul => [mul, Star, atom]));

    g.add_rule(rule!(mul => [atom]));

    g.add_rule(rule!(atom => [Int]));

    g.add_rule(rule!(atom => [ParenL, expr, ParenR]));

    (g, root)
}

#[allow(unused)]
fn parse(text: &str) -> bool {
    eprintln!("text = {}", text);

    let tokens = tokenize(text);
    let (grammar, non_term) = make_grammar();
    crate::parse::lr1::functions::parse(text.to_string(), tokens, non_term, grammar).is_some()
}

pub(crate) fn test() -> bool {
    let text = "(2 + 3 * 4) * (7)";

    let tokens = tokenize(text);
    let (grammar, non_term) = make_grammar();
    let tree = crate::parse::lr1::functions::parse(text.to_string(), tokens, non_term, grammar);
    eprintln!("{:?}", tree);
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_accept() {
        assert!(parse("1"));
        assert!(parse("2 + 3"));
        assert!(parse("4 * 5"));
        assert!(parse("2 + 3 * 4"));
        assert!(parse("4 * 5 + 6"));

        assert!(parse("(1)"));
        assert!(parse("(((((((((1)))))))))"));
        assert!(parse("(2 + 3)"));
        assert!(parse("(4 * 5)"));
        assert!(parse("(4 * (5))"));
        assert!(parse("(2 + 3) * 5"));
    }

    #[test]
    fn test_reject() {
        assert!(!parse(""));
        assert!(!parse("2 +"));
        assert!(!parse("++++****"));
        assert!(!parse("2 + + 3"));
        assert!(!parse("4 * 5 +"));
        assert!(!parse("("));
        assert!(!parse(")"));
        assert!(!parse("2 + ()"));
    }
}
