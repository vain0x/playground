use super::*;

/// 記号のファースト集合を計算するもの
pub(crate) struct FirstSet {
    nullable_symbols: Nullable,
    inner: HashMap<NonTerm, HashSet<Token>>,
}

impl FirstSet {
    pub(crate) fn new(grammar: &Grammar) -> FirstSet {
        let nullable_symbols = Nullable::new(grammar);
        let mut it = FirstSet {
            nullable_symbols,
            inner: HashMap::new(),
        };
        it.closure(grammar);
        it
    }

    fn is_nullable(&self, symbol: Symbol) -> bool {
        self.nullable_symbols.is_nullable(symbol)
    }

    fn first_non_term<'a>(&'a self, non_term: NonTerm) -> impl Iterator<Item = Token> + 'a {
        self.inner
            .get(&non_term)
            .map(|tokens| tokens.into_iter().cloned())
            .into_iter()
            .flatten()
    }

    pub(crate) fn first<'a>(&'a self, symbol: Symbol) -> impl Iterator<Item = Token> + 'a {
        debug_assert!(symbol.as_token().is_some() || symbol.as_non_term().is_some());

        symbol.as_token().into_iter().chain(
            symbol
                .as_non_term()
                .map(|non_term| self.first_non_term(non_term))
                .into_iter()
                .flatten(),
        )
    }

    pub(crate) fn first_of_symbols<'a>(
        &'a self,
        symbols: impl IntoIterator<Item = Symbol> + 'a,
    ) -> impl Iterator<Item = Token> + 'a {
        // FIXME: zero allocation?

        let mut tokens = HashSet::new();
        for symbol in symbols {
            tokens.extend(self.first(symbol));

            if !self.is_nullable(symbol) {
                break;
            }
        }
        tokens.into_iter()
    }

    /// s のファースト集合に t のファースト集合を併合する。
    /// 何らかの要素が追加されたら true を返す。
    fn merge_into(&mut self, s: NonTerm, t: Symbol) -> bool {
        // 注意: self への const 参照を見ながら mut 参照を更新するということはできない。
        //      for t in self.first(t) { self.first[s].insert(t); }
        //      身代わりパターンを使う。
        let mut tokens = replace(self.inner.entry(s).or_default(), HashSet::new());

        let mut modified = false;

        match t {
            Symbol::Token(t) => {
                modified |= tokens.insert(t);
            }
            Symbol::NonTerm(t) => {
                for t in self.first_non_term(t) {
                    modified |= tokens.insert(t);
                }
            }
        }

        debug_assert!(self.inner.get(&s).unwrap().is_empty());
        self.inner.insert(s, tokens);

        modified
    }

    fn closure(&mut self, grammar: &Grammar) {
        loop {
            let mut modified = false;

            for rule in grammar.rules() {
                let s = rule.source();

                for &t in rule.target() {
                    modified |= self.merge_into(s, t);

                    if !self.is_nullable(t) {
                        break;
                    }
                }
            }

            if !modified {
                break;
            }
        }
    }
}
