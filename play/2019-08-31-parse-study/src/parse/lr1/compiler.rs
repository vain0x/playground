use super::*;

pub(crate) struct Compiler {
    root: NonTerm,
    grammar: Grammar,

    /// 状態に対応するドットつき生成規則の集合へのマップ
    /// where closure(states[state]) = states[state]
    states: Vec<Vec<DotRule>>,

    first_set: FirstSet,

    /// 閉包の結果のメモ
    /// 閉包のもとになる生成規則の集合から、その閉包として作られた状態へのマップ。
    /// 同一の閉包を生成することがたまにあるため。
    /// closure_memo[&dot_rules] = state
    /// where closure(&dot_rules) = states[state]
    /// and   dot_rules: sorted and unique
    closure_memo: HashMap<Vec<DotRule>, StateId>,

    /// 閉包の結果のメモ。ただし先読みは除く (先読みトークンはすべてEOFになっている)。
    // closure_memo_la: HashMap<Vec<DotRule>, StateId>,

    /// DFA の辺集合
    /// edges[state][symbol] = next_state
    edges: Vec<HashMap<Symbol, StateId>>,

    /// 受理状態の集合
    accepts: HashSet<StateId>,

    /// 還元の集合
    reduces: Vec<Reduction>,
}

impl Compiler {
    pub(crate) fn new(root: NonTerm, grammar: Grammar) -> Self {
        let first_set = FirstSet::new(&grammar);

        Compiler {
            root,
            grammar,
            states: vec![],
            first_set,
            closure_memo: HashMap::new(),
            // closure_memo_la: HashMap::new(),
            edges: vec![],
            accepts: HashSet::new(),
            reduces: vec![],
        }
    }

    fn grammar(&self) -> &Grammar {
        &self.grammar
    }

    fn top_level_rule_id(&self) -> RuleId {
        self.grammar
            .rules()
            .into_iter()
            .position(|rule| rule.source() == self.root)
            .expect("根となる非終端記号は生成規則を持つはず")
    }

    fn find_closure_from_memo(&self, dot_rules: &mut Vec<DotRule>) -> Option<StateId> {
        // FIXME: 先読みだけが異なる集合は同一のクロージャにする?

        // 同一の生成規則集合から同一の閉包を生成することがしばしばあるので、メモ化する。
        dot_rules.sort_unstable();
        dot_rules.dedup();

        self.closure_memo.get(&*dot_rules).cloned()
    }

    fn add_state(
        &mut self,
        key: Vec<DotRule>,
        dot_rules: impl IntoIterator<Item = DotRule>,
    ) -> StateId {
        let mut dot_rules = dot_rules.into_iter().collect::<Vec<_>>();

        dot_rules.sort_unstable();
        dot_rules.dedup();
        self.states.push(dot_rules);
        let state = self.states.len() - 1;

        self.closure_memo.insert(key, state);
        eprintln!("State({:?}): {:?}", state, self.states[state]);

        state
    }

    fn closure(&mut self, mut dot_rules: Vec<DotRule>) -> StateId {
        if let Some(state) = self.find_closure_from_memo(&mut dot_rules) {
            eprintln!("reuse state");
            return state;
        }

        let dot_rules_key = dot_rules.clone();

        // i: dot_rules
        let mut dot_rules = dot_rules.into_iter().collect::<HashSet<_>>();
        let mut new_dot_rules = vec![];

        loop {
            eprintln!("closure({:?})", dot_rules);

            // for (X → α... ・Y β..., z) in i:
            for dot_rule in &dot_rules {
                let y = match dot_rule.next_symbol(self.grammar()) {
                    None | Some(Symbol::Token(_)) => continue,
                    Some(&Symbol::NonTerm(y)) => y,
                };

                // X の直後に出現することが先読みにより分かっているトークン
                let z = *dot_rule.token();

                for ri in 0..self.grammar().rules().len() {
                    if self.grammar().rules()[ri].source() != y {
                        continue;
                    }

                    // for w in first(βz):
                    // (w: Y を還元した直後に出現することが先読みにより分かるトークン)
                    let following = dot_rule
                        .after_next(self.grammar())
                        .iter()
                        .cloned()
                        .chain(iter::once(Symbol::Token(z)));
                    for w in self.first_set.first_of_symbols(following) {
                        // i ← i ∪ (Y → ・γ..., w)
                        let dot_rule = DotRule::new(ri, w);
                        new_dot_rules.push(dot_rule);
                    }
                }
            }

            let mut modified = false;

            for dot_rule in new_dot_rules.drain(..) {
                modified |= dot_rules.insert(dot_rule);
            }

            if !modified {
                break;
            }
        }

        self.add_state(dot_rules_key, dot_rules)
    }

    /// 状態 i で記号 Y を還元した後に遷移する状態を返す。なければ作る。
    fn trans(&mut self, i: StateId, y: Symbol) -> StateId {
        let mut dot_rules = vec![];

        for dot_rule in self.states[i].iter() {
            match dot_rule.next_symbol(self.grammar()) {
                Some(&x) if x == y => {
                    let dot_rule = dot_rule.clone().advance(self.grammar());
                    dot_rules.push(dot_rule);
                }
                _ => continue,
            }
        }

        let next_state = self.closure(dot_rules);

        eprintln!("trans({} ->{:?} {})", i, y, next_state);

        next_state
    }

    fn generate1(&mut self) {
        let mut new_accepts = vec![];
        let mut new_edges = vec![];

        loop {
            eprintln!("generate1...");

            let old_state_len = self.states.len();

            for i in 0..self.states.len() {
                // for (X → α... ・Y β..., z) in i:
                for si in 0..self.states[i].len() {
                    let y = match self.states[i][si].next_symbol(self.grammar()) {
                        None => continue,
                        Some(Symbol::Token(Token::Eof)) => {
                            new_accepts.push(i);
                            continue;
                        }
                        Some(&y) => y,
                    };

                    let j = self.trans(i, y);
                    new_edges.push((i, j, y));
                }
            }

            let mut modified = false;

            modified |= old_state_len != self.states.len();

            for (i, j, y) in new_edges.drain(..) {
                while i >= self.edges.len() {
                    self.edges.push(HashMap::new());
                }

                let old = self.edges[i].insert(y, j);

                modified |= old != Some(j);
            }

            for state in new_accepts.drain(..) {
                modified |= self.accepts.insert(state);
            }

            if !modified {
                break;
            }
        }
    }

    fn generate_reduces(&mut self) {
        let mut reduces = vec![];
        for i in 0..self.states.len() {
            // for (X → α... ・, z) in i:
            for dot_rule in self.states[i].iter() {
                if dot_rule.is_completed(self.grammar()) {
                    let x = dot_rule.source(self.grammar());
                    let count = dot_rule.len(self.grammar());
                    let z = *dot_rule.token();
                    reduces.push(Reduction {
                        state: i,
                        non_term: x,
                        count,
                        token: z,
                    })
                }
            }
        }
        self.reduces = reduces;
    }

    pub(crate) fn compile(mut self) -> Dfa {
        let top_level_rule_id = self.top_level_rule_id();

        let initial_state = {
            let dot_rules = vec![DotRule::new(top_level_rule_id, Token::Eof)];
            self.closure(dot_rules)
        };

        self.generate1();
        self.generate_reduces();

        Dfa {
            initial_state,
            state_count: self.states.len(),
            edges: self.edges,
            accepts: self.accepts,
            reduces: self.reduces,
        }
    }
}
