use super::*;
use crate::parser::{FinishedNode, StartedNode};

pub(crate) fn parse_root(p: &mut Parser) {
    let root = p.start_node();
    parse_exp(p);
    root.complete(p, SyntaxKind::RootExp);
}

fn parse_exp(p: &mut Parser) {
    parse_exp_add(p);

    if p.next() != SyntaxKind::EofToken {
        panic!("something not parsed {:?}", p.next())
    }
}

fn parse_exp_add(p: &mut Parser) -> FinishedNode {
    let mut exp = parse_exp_atom(p);

    loop {
        match p.next() {
            SyntaxKind::PlusToken | SyntaxKind::MinusToken => {
                let bin = exp.precede(p);
                {
                    // '+' または '-' をスキップ
                    p.bump();

                    // 右辺を解析
                    parse_exp_atom(p);
                }
                exp = bin.complete(p, SyntaxKind::BinExp);
            }
            _ => break,
        }
    }

    exp
}

fn parse_exp_atom(p: &mut Parser) -> FinishedNode {
    match p.next() {
        SyntaxKind::IntToken => {
            let literal = p.start_node();
            p.bump();
            literal.complete(p, SyntaxKind::LitExp)
        }
        // FIXME: エラー回復
        _ => panic!("atom 式が必要です"),
    }
}
