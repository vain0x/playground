use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct VarId(usize);

impl VarId {
    pub fn new(id: usize) -> VarId {
        VarId(id)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum TermKind {
    Var(VarId),
    // negation
    Neg,
    // conjunction
    Conj,
    // disjunction
    Disj,
}

#[derive(Clone, Debug)]
struct Term {
    kind: TermKind,
    children: Vec<Term>,
}

impl Term {
    pub fn var(var_id: VarId) -> Term {
        Term {
            kind: TermKind::Var(var_id),
            children: vec![],
        }
    }

    pub fn neg(self) -> Term {
        Term {
            kind: TermKind::Neg,
            children: vec![self],
        }
    }

    pub fn conj(terms: impl IntoIterator<Item = Term>) -> Term {
        Term {
            kind: TermKind::Conj,
            children: terms.into_iter().collect(),
        }
    }

    pub fn disj(terms: impl IntoIterator<Item = Term>) -> Term {
        Term {
            kind: TermKind::Disj,
            children: terms.into_iter().collect(),
        }
    }

    pub fn and(self, other: Term) -> Term {
        Term {
            kind: TermKind::Conj,
            children: vec![self, other],
        }
    }

    pub fn or(self, other: Term) -> Term {
        Term {
            kind: TermKind::Disj,
            children: vec![self, other],
        }
    }

    pub fn as_neg(&self) -> Option<&Term> {
        match self.kind {
            TermKind::Neg => {
                debug_assert_eq!(self.children.len(), 1);
                self.children.iter().next()
            }
            _ => None,
        }
    }

    pub fn free_vars(&self) -> Vec<VarId> {
        fn go(term: &Term, set: &mut BTreeSet<VarId>) {
            match term.kind {
                TermKind::Var(var_id) => {
                    set.insert(var_id);
                }
                _ => {
                    for child in &term.children {
                        go(child, set);
                    }
                }
            }
        }

        let mut vars = BTreeSet::new();
        go(self, &mut vars);
        vars.into_iter().collect()
    }
}

struct TseitinCnf {
    var_count: usize,
    cnf: Vec<Term>,
}

impl TseitinCnf {
    /// 与えられた論理式の積と同値な変数を生成する。
    fn add_tseitin_var(&mut self, terms: Vec<Term>) -> Term {
        self.var_count += 1;
        let var_id = VarId::new(self.var_count - 1);
        let var_term = Term::var(var_id);

        let neg_var = var_term.clone().neg();

        // X → ∧Y_i ≡ ∧(￢X ∨ Y_i)
        for term in &terms {
            self.cnf.push(neg_var.clone().or(term.clone()));
        }

        // NOTE: この条件はいらない。
        // X ← ∧Y_i ≡ X ∨ (∨ ￢Y_i)
        // let mut disj = vec![var_term.clone()];
        // for term in terms {
        //     disj.push(term.neg());
        // }
        // self.cnf.push(Term::disj(disj));

        var_term
    }

    /// 与えられた論理式に部分式として含まれる論理積を新しい変数で置換していく。
    /// 結果として、論理積を含まない論理式を生成する。
    /// 例:
    ///     `a | (b & (c & d))`
    ///     → `a | (b & e)`     (ただし e ⇔ c & d)
    ///     → `a | f`           (ただし f ⇔ b & e)
    fn tseitin(&mut self, term: Term) -> Term {
        match term.kind {
            TermKind::Var(..) | TermKind::Neg => term,
            TermKind::Conj => {
                let children = term
                    .children
                    .into_iter()
                    .map(|child| self.tseitin(child))
                    .collect();
                self.add_tseitin_var(children)
            }
            TermKind::Disj => {
                Term::disj(term.children.into_iter().map(|child| self.tseitin(child)))
            }
        }
    }
}

fn to_tseitin_cnf(term: Term) -> Term {
    let mut tseitin_cnf = TseitinCnf {
        var_count: term.free_vars().len(),
        cnf: vec![],
    };
    let body = tseitin_cnf.tseitin(term);
    tseitin_cnf.cnf.push(body);
    Term::conj(tseitin_cnf.cnf)
}

fn eval(term: &Term, assignments: &BTreeMap<VarId, bool>) -> bool {
    match term.kind {
        TermKind::Var(var_id) => *assignments.get(&var_id).unwrap(),
        TermKind::Neg => !eval(&term.as_neg().as_ref().unwrap(), assignments),
        TermKind::Conj => term.children.iter().all(|child| eval(child, assignments)),
        TermKind::Disj => term.children.iter().any(|child| eval(child, assignments)),
    }
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.kind {
            TermKind::Var(VarId(var_id)) => write!(f, "{}", var_id),
            TermKind::Neg => write!(f, "￢{}", self.as_neg().unwrap()),
            TermKind::Conj | TermKind::Disj => {
                let sep = match self.kind {
                    TermKind::Conj => "∧",
                    TermKind::Disj => "∨",
                    _ => unreachable!(),
                };

                write!(f, "(")?;
                for (i, child) in self.children.iter().enumerate() {
                    if i != 0 {
                        write!(f, "{}", sep)?;
                    }
                    write!(f, "{}", child)?;
                }
                write!(f, ")")
            }
        }
    }
}

fn main() {
    let term = Term::var(VarId::new(0)).and(
        Term::var(VarId::new(1)).or(Term::var(VarId::new(0)).or(Term::var(VarId::new(2)).neg())),
    );

    let alt = to_tseitin_cnf(term.clone());
    println!("{}", alt);

    let vars = alt.free_vars();
    let n = vars.len();
    let mut assignments = BTreeMap::new();
    for s in 0..(1 << n) {
        for i in 0..n {
            assignments.insert(vars[i], s & (1 << i) != 0);
        }

        let sat_term = eval(&term, &assignments);
        let sat_alt = eval(&alt, &assignments);

        // alt を充足する割り当ては term も充足する
        assert!(
            !sat_alt || sat_term,
            "term: {}, alt: {}, assignments = {:?}",
            sat_term,
            sat_alt,
            assignments
        );
    }
}
