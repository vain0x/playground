use super::*;

pub(crate) struct ParseTable {
    initial_state: StateId,
    inner: Vec<HashMap<Symbol, Action>>,
}

impl ParseTable {
    pub(crate) fn new(initial_state: StateId, state_count: usize) -> Self {
        ParseTable {
            initial_state,
            inner: vec![HashMap::new(); state_count],
        }
    }

    pub(crate) fn initial_state(&self) -> StateId {
        self.initial_state
    }

    pub(crate) fn on_token(&self, state: StateId, token: Token) -> Action {
        match self.inner[state].get(&Symbol::Token(token)) {
            None => Action::Error,
            Some(&action) => action,
        }
    }

    pub(crate) fn on_non_term(&self, state: StateId, non_term: NonTerm) -> Action {
        match self.inner[state].get(&Symbol::NonTerm(non_term)) {
            None => Action::Error,
            Some(&action) => action,
        }
    }

    fn add(&mut self, state: StateId, symbol: Symbol, action: Action) {
        let other = self.inner[state].insert(symbol, action);

        if let Some(other) = other {
            eprintln!("衝突 {:?}/{:?}", other, action);
        }
    }

    pub(crate) fn add_shift(&mut self, state: StateId, next_state: StateId, token: Token) {
        self.add(state, Symbol::Token(token), Action::Shift(next_state));
    }

    pub(crate) fn add_go(&mut self, state: StateId, next_state: StateId, non_term: NonTerm) {
        self.add(state, Symbol::NonTerm(non_term), Action::Go(next_state));
    }

    pub(crate) fn add_reduce(&mut self, r: Reduction) {
        self.add(
            r.state,
            Symbol::Token(r.look),
            Action::Reduce {
                non_term: r.non_term,
                count: r.count,
            },
        );
    }

    pub(crate) fn add_accept(&mut self, state: StateId) {
        for &token in Token::all() {
            self.add(state, Symbol::Token(token), Action::Accept);
        }
    }
}
