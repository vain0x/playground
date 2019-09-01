//! LL(1) 文法の構文解析器

use super::*;
use std::collections::VecDeque;

pub(crate) struct Ll1Parser {
    tokens: VecDeque<Token>,
}

impl Ll1Parser {
    pub(crate) fn new(tokens: impl Iterator<Item = Token>) -> Self {
        let tokens = tokens.collect();
        Ll1Parser { tokens }
    }

    pub(crate) fn next(&self) -> Token {
        if self.tokens.is_empty() {
            Token::Eof
        } else {
            self.tokens[0]
        }
    }

    pub(crate) fn advance(&mut self) {
        self.tokens.pop_front();
    }

    pub(crate) fn check(&mut self, token: Token) {
        if self.next() != token {
            panic!("Expected {:?} but {:?}", token, self.next());
        }

        self.advance();
    }
}

fn p_token(token: Token, _g: &Grammar, p: &mut Ll1Parser) {
    p.check(token);
}

fn p_non_term(non_term: NonTerm, g: &Grammar, p: &mut Ll1Parser) {
    for rule in g.rules() {
        if rule.source() != non_term {
            continue;
        }

        match rule.target().get(0) {
            None => {
                // 生成規則の右辺がない。NULL 導出可能。
                return;
            }
            Some(&Symbol::Token(token)) => {
                if p.next() != token {
                    continue;
                }

                for &t in rule.target().iter() {
                    p_symbol(t, g, p);
                }
                return;
            }
            Some(Symbol::NonTerm(_)) => {
                // S' のみ生成規則の右辺が非終端子で始まる。

                for &t in rule.target().iter() {
                    p_symbol(t, g, p);
                }
                return;
            }
        }
    }

    panic!("Couldn't parse")
}

fn p_symbol(symbol: Symbol, g: &Grammar, p: &mut Ll1Parser) {
    match symbol {
        Symbol::Token(token) => p_token(token, g, p),
        Symbol::NonTerm(non_term) => p_non_term(non_term, g, p),
    }
}

pub(crate) fn parse(tokens: Vec<Token>, non_term: NonTerm, g: &Grammar) {
    let mut p = Ll1Parser::new(tokens.into_iter());
    p_non_term(non_term, g, &mut p);
    assert_eq!(p.next(), Token::Eof);
}
