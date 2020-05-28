use super::parser::Parser;
use super::*;

fn eat_kind(kind: TokenKind, p: &mut Parser<'_>) -> bool {
    if p.next() == kind {
        p.bump();
        true
    } else {
        false
    }
}

fn expect_paren_l(p: &mut Parser<'_>) -> bool {
    if !eat_kind(TokenKind::ParenL, p) {
        p.error("Expected '('");
        return false;
    }
    true
}

fn expect_paren_r(p: &mut Parser<'_>) -> bool {
    if !eat_kind(TokenKind::ParenR, p) {
        // TODO: recover error
        p.error("Expected ')'");
        return false;
    }
    true
}

fn expect_dot(p: &mut Parser<'_>) -> bool {
    if !eat_kind(TokenKind::Dot, p) {
        p.error("Expected '.'");
        return false;
    }
    true
}

fn expect_ident(p: &mut Parser<'_>) -> bool {
    if !eat_kind(TokenKind::Ident, p) {
        p.error("Expected identifier");
        return false;
    }
    true
}

fn eat_keyword(keyword: Keyword, p: &mut Parser<'_>) -> bool {
    eat_kind(TokenKind::Keyword(keyword), p)
}

fn expect_keyword(keyword: Keyword, p: &mut Parser<'_>) -> bool {
    if !eat_kind(TokenKind::Keyword(keyword), p) {
        p.error(&format!("Expected '{}'", keyword.as_str()));
        return false;
    }
    true
}

fn eat_paren_keyword(keyword: Keyword, p: &mut Parser<'_>) -> bool {
    match p.next2() {
        (TokenKind::ParenL, TokenKind::Keyword(k)) if k == keyword => {
            p.advance(2);
            true
        }
        _ => false,
    }
}

fn eat_val_ty(p: &mut Parser<'_>) -> bool {
    match p.next() {
        TokenKind::Keyword(keyword) if keyword.is_val_ty() => {
            p.bump();
            true
        }
        _ => false,
    }
}

fn parse_eof(p: &mut Parser<'_>) {
    if p.next() != TokenKind::Eof {
        p.error("Expected EOF");
        while p.next() != TokenKind::Eof {
            p.bump();
        }
    }
    p.bump();
}

pub(crate) fn parse_root(p: &mut Parser<'_>) {
    let root_node = p.start_node();

    parse_module(p);
    parse_eof(p);

    root_node.complete(SynKind::Root, p);
}

fn parse_module(p: &mut Parser<'_>) {
    let module_node = p.start_node();

    expect_paren_l(p);

    if !eat_keyword(Keyword::Module, p) {
        p.error("Expected 'module'");
        return;
    }

    while parse_section(p) {}

    expect_paren_r(p);

    module_node.complete(SynKind::ModuleDecl, p);
}

fn parse_section(p: &mut Parser<'_>) -> bool {
    if p.next() != TokenKind::ParenL {
        return false;
    }

    match p.nth_kind(1) {
        TokenKind::Keyword(Keyword::Func) => {
            parse_func_decl(p);
        }
        TokenKind::Keyword(Keyword::Export) => {
            parse_export_decl(p);
        }
        _ => {
            p.error("Expected keyword");
        }
    }

    true
}

fn parse_func_decl(p: &mut Parser<'_>) {
    let node = p.start_node();

    expect_paren_l(p);
    expect_keyword(Keyword::Func, p);

    // func-id (TODO: allow index)
    eat_kind(TokenKind::Ident, p);

    // TODO: allow inline import/export, type use, etc.

    // signature (TODO: allow (param ..))
    loop {
        match p.next2() {
            (TokenKind::ParenL, TokenKind::Keyword(Keyword::Result)) => {
                parse_result_ty(p);
            }
            _ => break,
        }
    }

    // instr
    while parse_instr(p) {}

    expect_paren_r(p);

    node.complete(SynKind::FuncDecl, p);
}

fn parse_result_ty(p: &mut Parser<'_>) {
    let node = p.start_node();

    expect_paren_l(p);
    expect_keyword(Keyword::Result, p);
    eat_val_ty(p);
    eat_kind(TokenKind::Ident, p);
    expect_paren_r(p);

    node.complete(SynKind::ResultTy, p);
}

fn parse_instr(p: &mut Parser<'_>) -> bool {
    match p.next3() {
        (TokenKind::Keyword(Keyword::I32), TokenKind::Dot, TokenKind::Keyword(Keyword::Const)) => {
            parse_const_instr(p);
            true
        }
        (TokenKind::Keyword(Keyword::I32), TokenKind::Dot, TokenKind::Keyword(Keyword::Add)) => {
            let instr_node = p.start_node();
            let op_node = p.start_node();
            expect_keyword(Keyword::I32, p);
            expect_dot(p);
            expect_keyword(Keyword::Add, p);
            op_node.complete(SynKind::Op, p);
            instr_node.complete(SynKind::Instr, p);
            true
        }
        _ => false,
    }
}

fn parse_const_instr(p: &mut Parser<'_>) {
    let instr_node = p.start_node();

    let op_node = p.start_node();
    expect_keyword(Keyword::I32, p);
    expect_dot(p);
    expect_keyword(Keyword::Const, p);
    op_node.complete(SynKind::Op, p);

    if !parse_val(p) {
        p.error("Expected val");
    }

    instr_node.complete(SynKind::Instr, p);
}

fn parse_val(p: &mut Parser<'_>) -> bool {
    match p.next() {
        TokenKind::Int => {
            let node = p.start_node();
            p.bump();
            node.complete(SynKind::Val, p);
            true
        }
        _ => false,
    }
}

fn parse_export_decl(p: &mut Parser<'_>) {
    let node = p.start_node();

    expect_paren_l(p);
    expect_keyword(Keyword::Export, p);

    // name
    eat_kind(TokenKind::Str, p);

    // export-desc
    match p.next2() {
        (TokenKind::ParenL, TokenKind::Keyword(Keyword::Func)) => {
            let node = p.start_node();
            expect_paren_l(p);
            expect_keyword(Keyword::Func, p);
            expect_ident(p);
            node.complete(SynKind::ExportDesc, p);
        }
        _ => p.error("export desc not supported other than (func ..)"),
    }

    expect_paren_r(p);

    node.complete(SynKind::ExportDecl, p);
}
