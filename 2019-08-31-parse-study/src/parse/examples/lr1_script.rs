//! LR(1) 文法のスクリプト

use super::*;
use crate::parse::examples::tokenize::tokenize;
use Token::*;

macro_rules! rule {
    ($s:ident => [$($t:expr),*]) => {{
        #[allow(unused_mut)]
        let mut target = vec![];
        $(target.push($t.into());)*

        ($s).to_rule(target)
    }}
}

fn make_grammar() -> (Grammar, NonTerm) {
    let mut g = Grammar::new();

    let root = NonTerm::new("root");
    let semi = NonTerm::new("semi");
    let expr = NonTerm::new("expr");
    let cmp = NonTerm::new("cmp");
    let add = NonTerm::new("add");
    let mul = NonTerm::new("mul");
    let suffix = NonTerm::new("suffix");
    let atom = NonTerm::new("atom");
    let obj_body = NonTerm::new("obj_body");
    let obj_pair = NonTerm::new("obj_pair");
    let block = NonTerm::new("block");
    let if_expr = NonTerm::new("if_expr");

    g.add_rule(rule!(root => [semi, Eof]));

    g.add_rule(rule!(semi => [expr, Semi, semi]));

    g.add_rule(rule!(semi => [if_expr, semi]));

    g.add_rule(rule!(semi => [expr]));

    g.add_rule(rule!(semi => []));

    g.add_rule(rule!(expr => [Ident, Lt, Hyphen, expr]));

    g.add_rule(rule!(expr => [cmp]));

    g.add_rule(rule!(cmp => [cmp, Eq, Eq, add]));

    g.add_rule(rule!(cmp => [add]));

    g.add_rule(rule!(add => [add, Plus, mul]));

    g.add_rule(rule!(add => [add, Hyphen, mul]));

    g.add_rule(rule!(add => [mul]));

    g.add_rule(rule!(mul => [mul, Star, suffix]));

    g.add_rule(rule!(mul => [suffix]));

    g.add_rule(rule!(suffix => [suffix, ParenL, expr, ParenR]));

    g.add_rule(rule!(suffix => [suffix, BracketL, expr, BracketR]));

    g.add_rule(rule!(suffix => [atom]));

    g.add_rule(rule!(atom => [Int]));

    g.add_rule(rule!(atom => [Ident]));

    g.add_rule(rule!(atom => [ParenL, expr, ParenR]));

    g.add_rule(rule!(atom => [block]));

    g.add_rule(rule!(atom => [if_expr]));

    g.add_rule(rule!(obj_body => [obj_pair, Comma, obj_body]));

    g.add_rule(rule!(obj_body => [obj_pair]));

    g.add_rule(rule!(obj_pair => [Ident, Colon, expr]));

    g.add_rule(rule!(block => [BraceL, semi, BraceR]));

    g.add_rule(rule!(if_expr => [If, expr, block]));

    g.add_rule(rule!(if_expr => [If, expr, block, Else, block]));

    g.add_rule(rule!(if_expr => [If, expr, block, Else, if_expr]));

    (g, root)
}

pub(crate) fn parse(text: &str) -> String {
    let text = text.to_string();

    let tokens = tokenize(&text);
    let (grammar, non_term) = make_grammar();
    let tree = crate::parse::lr1::functions::parse(text, tokens, non_term, grammar);
    tree.map(|tree| format!("{:?}", tree))
        .unwrap_or("Syntax error".to_string())
}

pub(crate) fn test() {
    let table = [
        r#"// int
            1
        "#,
        r#"// ident
            answer
        "#,
        r#"// semi
            1; 2; 3
        "#,
        r#"// eq
            0 == 1
        "#,
        r#"// add + mul
            2 + 3 * 4
        "#,
        r#"// call
            f(x)
        "#,
        r#"// add + mul + call
            f(x) + g(y) * h(z)
        "#,
        r#"// call call
            f(x)(y)
        "#,
        r#"// call + index
            f(x)[y](z)
        "#,
        r#"// block
            {
                1; 2
            }
        "#,
        r#"// if
            if true {}
            if false {}
        "#,
        r#"// if-else
            if ok {
                0
            } else {
                1
            }
        "#,
        r#"// else-if
            if x == 1 {
                1
            } else if x == 2 {
                2
            } else if x == 3 {
                3
            } else {
                4
            }
        "#,
        r#"
            x <- 0;
            if { x } == { 0 } {
                x <- {
                    if x == 0 {
                        1
                    } else {
                        0
                    }
                }
            }
            x <- if x == 1 {
                2
            } else {
                0
            }
        "#,
    ];

    for &text in table.iter() {
        print!("---- ---- ---- ----\n");
        print!("{}\n", text.trim_end());

        print!("---- ---- ---- ----\n");
        let result = parse(text);
        print!("{}\n", result);
    }
}
