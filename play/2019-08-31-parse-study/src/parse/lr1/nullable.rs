use super::*;

/// 記号が空導出可能か判定するもの
pub(crate) struct Nullable {
    inner: HashSet<NonTerm>,
}

impl Nullable {
    pub(crate) fn new(grammar: &Grammar) -> Self {
        let mut it = Nullable {
            inner: HashSet::new(),
        };
        it.closure(grammar);
        it
    }

    fn is_nullable_non_term(&self, non_term: NonTerm) -> bool {
        self.inner.contains(&non_term)
    }

    pub(crate) fn is_nullable(&self, symbol: Symbol) -> bool {
        match symbol {
            Symbol::Token(_) => false,
            Symbol::NonTerm(non_term) => self.is_nullable_non_term(non_term),
        }
    }

    fn closure(&mut self, grammar: &Grammar) {
        loop {
            let mut modified = false;

            for rule in grammar.rules() {
                let s = rule.source();
                if self.is_nullable_non_term(s) {
                    continue;
                }

                let is_nullable = rule.target().iter().all(|&symbol| self.is_nullable(symbol));
                if !is_nullable {
                    continue;
                }

                modified |= self.inner.insert(s);
                assert!(modified);
            }

            if !modified {
                break;
            }
        }
    }
}
