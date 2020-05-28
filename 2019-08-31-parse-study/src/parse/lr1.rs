//! LR(1) 文法の構文解析器

pub(crate) mod compiler;
pub(crate) mod dfa;
pub(crate) mod dot_rule;
pub(crate) mod first_set;
pub(crate) mod nullable;
pub(crate) mod parse_table;
pub(crate) mod parse_tree;
pub(crate) mod parser;

pub(crate) use compiler::Compiler;
pub(crate) use dfa::Dfa;
pub(crate) use dot_rule::DotRule;
pub(crate) use first_set::FirstSet;
pub(crate) use nullable::Nullable;
pub(crate) use parse_table::ParseTable;
pub(crate) use parse_tree::ParseTree;
pub(crate) use parser::{Event, Parser};

use super::*;
use std::cmp::min;
use std::collections::{HashMap, HashSet};
use std::iter;
use std::mem::replace;

type StateId = usize;

type RuleId = usize;

/// 還元動作
#[derive(Clone, Copy, Debug)]
pub(crate) struct Reduction {
    state: StateId,
    non_term: NonTerm,
    count: usize,
    look: Token,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Action {
    Error,
    Accept,
    Shift(StateId),
    Go(StateId),
    Reduce { non_term: NonTerm, count: usize },
}

pub(crate) mod functions {
    use super::*;

    pub(crate) fn parse(
        text: String,
        tokens: Vec<TokenData>,
        root: NonTerm,
        grammar: Grammar,
    ) -> Option<ParseTree> {
        let compiler = Compiler::new(root, grammar);
        let dfa = compiler.compile();
        let parse_table = dfa.into_parse_table();
        let parser = Parser::new(tokens.clone(), parse_table);
        let events = parser.parse()?;
        let tree = ParseTree::from_events(text, tokens, events);
        Some(tree)
    }
}
