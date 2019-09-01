use super::*;
use std::collections::BTreeSet;
use std::mem::swap;

/// NFA のエッジのラベル
#[derive(Clone, Debug)]
enum SlowNfaLabel {
    /// ε遷移
    Eps,

    /// 入力を1文字読んで遷移
    Char(char),
}

/// 単純で低速な NFA の実装
#[derive(Clone, Debug)]
struct SlowNfa {
    /// states[u][i] = (c, v) : 状態 u で入力 c を受け取ったら次の状態 v に遷移する
    states: Vec<Vec<(SlowNfaLabel, usize)>>,
}

impl SlowNfa {
    fn new() -> Self {
        Self { states: vec![] }
    }

    /// 状態を追加して状態IDを返す。
    fn add_state(&mut self) -> usize {
        let v = self.states.len();
        self.states.push(vec![]);
        v
    }

    /// 1文字消費する遷移 u→v を追加する。
    fn add_edge(&mut self, u: usize, v: usize, c: char) {
        self.states[u].push((SlowNfaLabel::Char(c), v));
    }

    /// ε遷移 u→v を追加する。
    fn add_edge_eps(&mut self, u: usize, v: usize) {
        self.states[u].push((SlowNfaLabel::Eps, v));
    }

    /// 開始状態を entry_v から入力 inputs を読むとき最終的な状態集合を返す。
    fn run(&self, entry_v: usize, inputs: &str) -> BTreeSet<usize> {
        /// u からちょうど1回 ε 遷移した先を out に追加する。
        fn eps(u: usize, nfa: &SlowNfa, out: &mut BTreeSet<usize>) {
            for ei in 0..nfa.states[u].len() {
                match nfa.states[u][ei] {
                    (SlowNfaLabel::Eps, v) => {
                        out.insert(v);
                    }
                    _ => {}
                }
            }
        }

        /// 状態集合 set に、任意回 ε 遷移した先の状態をすべて追加する。
        fn closure(set: &mut BTreeSet<usize>, nfa: &SlowNfa) {
            let mut next = set.clone();

            loop {
                for &u in set.iter() {
                    eps(u, nfa, &mut next);
                }

                if *set == next {
                    break;
                }

                set.extend(&next);
            }
        }

        let mut set = BTreeSet::new();
        set.insert(entry_v);

        // 初期状態集合を計算する。
        let mut next = set.clone();
        closure(&mut next, self);

        eprintln!("ε {:?} -> {:?}", set, next);
        swap(&mut set, &mut next);

        for c in inputs.chars() {
            // 1文字分の遷移を行う。

            next.clear();

            for &u in set.iter() {
                for ei in 0..self.states[u].len() {
                    match self.states[u][ei] {
                        (SlowNfaLabel::Char(label_c), v) if label_c == c => {
                            next.insert(v);
                        }
                        _ => {}
                    }
                }
            }

            closure(&mut next, self);

            eprintln!("{:?} {:?} -> {:?}", c, set, next);
            swap(&mut set, &mut next);
        }

        set
    }
}

/// NFA のシミュレーションにより正規表現を実行する
pub fn run_term_with_slow_nfa(term: &Term, input: &str) -> bool {
    fn add_term(term: &Term, u: usize, nfa: &mut SlowNfa) -> usize {
        match term {
            Term::Null => {
                let v = nfa.add_state();
                nfa.add_edge_eps(u, v);
                v
            }
            &Term::Char(c) => {
                let v = nfa.add_state();
                nfa.add_edge(u, v, c);
                v
            }
            Term::Conj(l, r) => {
                let v = add_term(l, u, nfa);
                add_term(r, v, nfa)
            }
            Term::Disj(l, r) => {
                // 分岐点
                let v = nfa.add_state();
                nfa.add_edge_eps(u, v);

                let lv = add_term(l, v, nfa);
                let rv = add_term(r, v, nfa);

                // 合流点
                let w = nfa.add_state();
                nfa.add_edge_eps(lv, w);
                nfa.add_edge_eps(rv, w);

                w
            }
            Term::Many(t) => {
                // 終着点
                // t を0回以上繰り返した後にここにいる。
                let v = nfa.add_state();
                nfa.add_edge_eps(u, v);

                // もう1回繰り返すことを許可する。
                let tv = add_term(t, v, nfa);
                nfa.add_edge_eps(tv, v);

                v
            }
        }
    }

    let mut nfa = SlowNfa::new();
    let entry = nfa.add_state();
    let exit = add_term(term, entry, &mut nfa);

    eprintln!("nfa {:?} entry={} exit={}", nfa, entry, exit);

    let exits = nfa.run(entry, input);

    // 状態のいずれかが受理状態に遷移していればOK
    exits.contains(&exit)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        fn t(pattern: &str, accepts: Vec<&str>, rejects: Vec<&str>) {
            let term = Term::parse(pattern);

            for input in accepts {
                assert!(
                    run_term_with_slow_nfa(&term, input),
                    "Pattern {} expected to accept {} but rejected",
                    pattern,
                    input
                );
            }

            for input in rejects {
                assert!(
                    !run_term_with_slow_nfa(&term, input),
                    "Pattern {} expected to reject {} but accepted",
                    pattern,
                    input
                );
            }
        }

        t("1*", vec!["", "1", "11111"], vec!["0"]);
        t("0|1", vec!["0", "1"], vec!["", "00", "11", "2"]);
        t("[0123]*[abc]*", vec!["", "1a", "0102caba"], vec!["a1", "z"]);
    }
}
