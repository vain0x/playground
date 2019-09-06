//! LR(0) 文法の構文解析器

use super::*;
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Debug, Formatter, Write};
use std::rc::Rc;

/// ドットつきの生成規則。LR(0)アイテム。
///
/// ドットは解析がどこまで完了したかを表す。
///
/// ドットの位置が左にある生成規則から順番に使って、
/// 少しずつ「ドットを右に動かしていく」ことで構文解析を進める。(たぶん)
/// ドットを右に動かすには、次の記号がトークンならシフトすればいい。(たぶん)
/// 非終端記号 X なら、それの生成規則 (X → α...) に入って、右辺にあるドットを1つ進むまで再帰的に解析を行う。
/// このときスタック上に記号列 α に対応する状態が並んでいるので、これらをポップして代わりに X を積む。(還元)
/// 元の生成規則に戻ると、次の記号 X がまさにスタックの一番上にあるので、ドットを右に移動した状態に遷移 (移動) できる。(たぶん)
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct DotRule {
    rule: Rule,
    dot: usize,
}

/// 状態の識別子
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct State(usize);

impl DotRule {
    fn source(&self) -> NonTerm {
        self.rule.source()
    }

    fn len(&self) -> usize {
        self.rule.target().len()
    }

    /// ドットの直後にある記号
    fn next_symbol(&self) -> Option<Symbol> {
        self.rule.target().get(self.dot).cloned()
    }

    /// 解析完了状態 (ドットが右端にある状態)？
    fn is_completed(&self) -> bool {
        self.next_symbol().is_none()
    }

    /// ドットを1つ進める
    fn advance(mut self) -> DotRule {
        if !self.is_completed() {
            self.dot += 1;
        }
        self
    }
}

impl From<State> for usize {
    fn from(state: State) -> usize {
        state.0
    }
}

impl From<usize> for State {
    fn from(i: usize) -> State {
        State(i)
    }
}

impl From<Rule> for DotRule {
    fn from(rule: Rule) -> DotRule {
        DotRule { rule, dot: 0 }
    }
}

impl Debug for DotRule {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // ドットつき生成規則をデバッグ用に文字列表示する。

        f.write_fmt(format_args!("{:?} →", self.source()))?;

        for i in 0..self.rule.target().len() {
            f.write_char(' ')?;

            if i == self.dot {
                f.write_char('・')?;
            }

            f.write_fmt(format_args!("{:?}", self.rule.target()[i]))?;
        }

        if self.is_completed() {
            f.write_char('・')?;
        }

        Ok(())
    }
}

/// 構文解析中の操作。
#[derive(Clone, Copy, Debug)]
enum Action {
    /// 構文解析が失敗したとき
    Error,
    /// 受理
    Accept,
    /// シフト (i)
    /// 次のトークンを読んで、状態 i をスタックに置く。
    /// 状態 i は読んだトークンの右側にドットがある状態を指している？
    Shift(State),
    /// 還元 (s, k)
    /// スタック上の k 個 (= 生成規則の右辺の記号数) の状態をポップして、
    /// スタック上の最上段の状態と非終端記号 s から導かれるアクション Go(j) を実行する。
    Reduce { source: Symbol, count: usize },
    /// 移動(j)
    /// 状態 j をスタックに置く。
    /// この操作は還元の直後に実行される。
    Go(State),
}

/// LR(0)文法の構文解析器を生成するもの。
struct Lr0ParserCompiler {
    root: NonTerm,
    grammar: Grammar,
}

/// LR(0)文法の構文解析器
struct Lr0Parser {
    entry: State,
    table: HashMap<(State, Symbol), Action>,
    grammar: Grammar,
}

impl Lr0ParserCompiler {
    fn new(root: NonTerm, grammar: Grammar) -> Self {
        Lr0ParserCompiler { root, grammar }
    }

    fn closure(&mut self, dot_rules: &mut HashSet<DotRule>) {
        // closure(i) =
        //      for 状態iの中の各項 (X → ... ・Y ...) について:
        //          for 生成規則 (Y → ...) について:
        //              i ← i ∪ {(Y → ・...)}
        //      closure(i)      // closure(i) = i になるまで続ける

        let mut non_terms = vec![];

        loop {
            let mut modified = false;

            for dot_rule in dot_rules.iter() {
                match dot_rule.next_symbol() {
                    None | Some(Symbol::Token(_)) => continue,
                    Some(Symbol::NonTerm(non_term)) => non_terms.push(non_term),
                }
            }

            while let Some(non_term) = non_terms.pop() {
                for rule in self.grammar.rules() {
                    if rule.source() == non_term {
                        modified |= dot_rules.insert(DotRule::from(rule.clone()));
                    }
                }
            }

            if !modified {
                break;
            }
        }
    }

    fn trans(&mut self, _i: State, y: Symbol, i_rules: &[DotRule]) -> Rc<Vec<DotRule>> {
        // trans(i, Y):
        //      closure({ (X → ...Y・...) | (X → ...・Y...) in i })

        let mut dot_rules = i_rules
            .iter()
            .filter_map(|rule| {
                if rule.next_symbol() == Some(y) {
                    Some(rule.clone().advance())
                } else {
                    None
                }
            })
            .collect::<HashSet<_>>();
        self.closure(&mut dot_rules);

        let mut dot_rules = dot_rules.into_iter().collect::<Vec<_>>();
        dot_rules.sort();

        Rc::new(dot_rules)
    }

    fn compile(mut self) -> Lr0Parser {
        let top_level_rule = self
            .grammar
            .rules()
            .into_iter()
            .filter(|rule| rule.source() == self.root)
            .next()
            .expect("終端記号は生成規則を持つはず")
            .clone();

        // 状態集合
        let mut t = HashSet::new();
        // 状態とドットつき規則の対応付け
        let mut rules = HashMap::new();
        let mut map = HashMap::new();
        // 受理状態の集合
        let mut a = HashSet::new();
        // エッジの集合
        let mut e = HashSet::new();
        // 還元の集合
        let mut r = vec![];

        // 開始状態を追加する。
        let entry = State(0);
        {
            let mut dot_rules = HashSet::new();
            dot_rules.insert(DotRule::from(top_level_rule));
            self.closure(&mut dot_rules);
            let mut dot_rules = dot_rules.into_iter().collect::<Vec<_>>();
            dot_rules.sort();
            let dot_rules = Rc::new(dot_rules);

            eprintln!("{:?}", entry);
            for dot_rule in dot_rules.iter() {
                eprintln!("  {:?}", dot_rule);
            }

            map.insert(dot_rules.clone(), entry);
            rules.insert(entry, dot_rules);
        };
        t.insert(entry);

        eprintln!("entry({:?})", entry);

        // DFA にトークンによる遷移 (シフト) を追加する。
        loop {
            let mut new_states = vec![];
            let mut new_edges = vec![];

            eprintln!("{:?}", t);

            for &i in t.iter() {
                let n = match rules.get(&i) {
                    None => continue,
                    Some(dot_rules) => dot_rules.len(),
                };

                for ri in 0..n {
                    // 次の記号が Y のとき。
                    // i から Y で遷移した先の状態 j を生成して状態集合に加える。

                    eprintln!("rule({:?})", rules[&i][ri]);

                    let y = match rules[&i][ri].next_symbol() {
                        None => continue,
                        Some(Symbol::Token(Token::Eof)) => {
                            a.insert(i);
                            continue;
                        }
                        Some(y) => y,
                    };

                    let dot_rules = self.trans(i, y, &*rules[&i]);

                    let new_state = State(map.len());
                    let j = *map.entry(dot_rules.clone()).or_insert(new_state);

                    if j == new_state {
                        rules.insert(j, dot_rules);

                        eprintln!("{:?}:", j);
                        for dot_rule in rules[&j].iter() {
                            eprintln!("  {:?}", dot_rule);
                        }
                    }

                    new_states.push(j);
                    new_edges.push((i, j, y));
                }
            }

            let mut modified = false;

            for state in new_states.drain(..) {
                modified |= t.insert(state);
            }

            for edge in new_edges.drain(..) {
                modified |= e.insert(edge);
            }

            if !modified {
                break;
            }
        }

        // DFA に非終端記号による遷移 (還元) を追加する。
        for &i in t.iter() {
            // 状態 i に含まれる各規則 (X → ...・) につき、状態 i において終端記号 X を還元できるとみなす。

            let n = match rules.get(&i) {
                None => continue,
                Some(dot_rules) => dot_rules.len(),
            };

            for ri in 0..n {
                let dot_rule = &rules[&i][ri];
                if dot_rule.is_completed() {
                    r.push((i, dot_rule.source(), dot_rule.len()));
                }
            }
        }

        // DFA を構文解析表に変換する。
        let mut table = HashMap::new();

        for &(i, j, y) in e.iter() {
            let action = match y {
                Symbol::Token(_) => Action::Shift(j),
                Symbol::NonTerm(_) => Action::Go(j),
            };
            let other = table.insert((i, y), action);

            if let Some(other) = other {
                eprintln!("衝突 ({:?})", (i, j, y, other));
            }
        }

        for &(i, non_term, count) in r.iter() {
            let reduce = Action::Reduce {
                source: Symbol::NonTerm(non_term),
                count,
            };

            // 次の字句に関係なく還元操作を行う。
            for &token in Token::all() {
                let other = table.insert((i, Symbol::Token(token)), reduce);

                if let Some(other) = other {
                    eprintln!("衝突 ({:?})", (non_term, i, token, other));
                }
            }
        }

        for &i in a.iter() {
            // 次の字句に関係なく受理操作を行う。
            for &token in Token::all() {
                let other = table.insert((i, Symbol::Token(token)), Action::Accept);

                if let Some(other) = other {
                    eprintln!("衝突 ({:?})", (i, token, Action::Accept, other));
                }
            }
        }

        eprintln!("table = {:?}", table);

        Lr0Parser {
            entry,
            table,
            grammar: self.grammar,
        }
    }
}

impl Lr0Parser {
    fn parse(&self, tokens: &[Token]) -> bool {
        let mut stack = vec![self.entry];

        let mut ti = 0;
        loop {
            let state = match stack.last() {
                None => unreachable!(),
                Some(&state) => state,
            };

            let token = match tokens.get(ti) {
                None => {
                    eprintln!("構文エラー");
                    return false;
                }
                Some(&token) => token,
            };

            let action = match self.table.get(&(state, Symbol::Token(token))) {
                None => {
                    eprintln!("行き詰まり");
                    return false;
                }
                Some(&action) => action,
            };

            eprintln!(
                "状態({:?}), 字句({:?}), 操作({:?}), スタック({:?})",
                state, token, action, stack
            );

            match action {
                Action::Error => {
                    eprintln!("構文エラー");
                    return false;
                }
                Action::Accept => return true,
                Action::Shift(next_state) => {
                    stack.push(next_state);
                    ti += 1;
                    continue;
                }
                Action::Reduce { source, count } => {
                    // いま生成規則 source → t1 t2 ... tN の解析が完了している状態。
                    // t1, t2, ... に対応する状態をスタックから除去する。
                    // 結果的に source の解析を開始する前の状態に (ti 以外) 巻き戻る。
                    for _ in 0..count {
                        let s = stack.pop();
                        assert!(s.is_some());
                    }

                    // source の解析が完了したので、ドットが source を飛び越えた先の状態に遷移する。
                    let state = match stack.last() {
                        None => unreachable!(),
                        Some(&state) => state,
                    };

                    let next_state = match self.table.get(&(state, source)) {
                        Some(&Action::Go(next_state)) => next_state,
                        action => unreachable!(
                            "還元後の操作は Go でなければいけない ({:?})",
                            action
                        ),
                    };

                    stack.push(next_state);

                    eprintln!(
                        "還元({:?}, {}) {:?} → {:?}",
                        source, count, state, next_state
                    );
                    continue;
                }
                Action::Go(..) => {
                    unreachable!("Go 操作は終端記号によっては引き起こされない")
                }
            }
        }
    }
}

pub(crate) fn parse(tokens: Vec<Token>, root: NonTerm, grammar: Grammar) -> bool {
    let compiler = Lr0ParserCompiler::new(root, grammar);
    let parser = compiler.compile();
    parser.parse(&tokens)
}
