use super::*;
use std::cmp::Reverse;

/// 文法から生成されたDFA
pub(crate) struct Dfa {
    pub(super) initial_state: StateId,
    pub(super) state_count: usize,
    pub(super) edges: Vec<HashMap<Symbol, StateId>>,
    pub(super) accepts: HashSet<StateId>,
    pub(super) reduces: Vec<Reduction>,
}

impl Dfa {
    fn resolve_reduce_reduce_conflict(&mut self) {
        // 同じ状態から複数の還元があるときは、最長のものを1つ選ぶ。
        self.reduces
            .sort_by_key(|r| (r.state, r.non_term, Reverse(r.count)));
        self.reduces.dedup_by_key(|r| (r.state, r.non_term));
    }

    fn resolve_accept_conflict(&mut self) {
        let accepts = self.accepts.clone();

        // 受理状態ではシフトや移動を行わない。
        for &state in accepts.iter() {
            self.edges[state].clear();
        }

        // 受理状態では還元しない。
        self.reduces.retain(|r| !accepts.contains(&r.state));
    }

    pub(crate) fn into_parse_table(mut self) -> ParseTable {
        let mut table = ParseTable::new(self.initial_state, self.state_count);

        self.resolve_reduce_reduce_conflict();
        self.resolve_accept_conflict();

        for i in 0..self.edges.len() {
            for (&symbol, &next_state) in self.edges[i].iter() {
                match symbol {
                    Symbol::Token(token) => table.add_shift(i, next_state, token),
                    Symbol::NonTerm(non_term) => table.add_go(i, next_state, non_term),
                }
            }
        }

        for r in self.reduces.iter() {
            table.add_reduce(r.state, r.non_term, r.count);
        }

        for &state in self.accepts.iter() {
            table.add_accept(state);
        }

        table
    }
}
