use crate::{coord::*, formula::*};
use std::{collections::HashSet, fmt::Debug};

#[derive(Clone, Copy, PartialEq)]
pub(crate) enum CellValue {
    Null,
    Number(f64),

    // Bad
    Invalid,
    Recursive,
    DividedByZero,
}

impl CellValue {
    pub(crate) fn to_string(&self) -> String {
        format!("{:?}", self)
    }
}

impl Debug for CellValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CellValue::Null => write!(f, ""),
            CellValue::Number(value) => write!(f, "{value:.2}"),
            CellValue::Invalid => write!(f, "#VALUE!"),
            CellValue::Recursive => write!(f, "#REF!"),
            CellValue::DividedByZero => write!(f, "#DIV/0!"),
        }
    }
}

#[derive(Clone)]
pub(crate) enum CellInput {
    Null,
    Number(f64),
    Formula(Formula),
}

impl CellInput {
    fn parse(s: &str) -> Option<Self> {
        if s.starts_with('=') {
            let f = Formula::parse(s[1..].trim())?;
            return Some(CellInput::Formula(f));
        }

        if s.trim().is_empty() {
            return Some(CellInput::Null);
        }

        let value = s.parse::<f64>().ok()?;
        Some(CellInput::Number(value))
    }
}

#[derive(Clone, Default)]
struct FormulaDeps {
    refs: Vec<Coord>,
    ranges: Vec<CoordRange>,
}

impl FormulaDeps {
    #[allow(unused)]
    fn is_empty(&self) -> bool {
        self.refs.is_empty() && self.ranges.is_empty()
    }

    fn iter_cells<'a>(&'a self) -> impl Iterator<Item = Coord> + 'a {
        self.ranges
            .iter()
            .flat_map(|range| range.iter_cells())
            .chain(self.refs.iter().copied())
    }

    fn clear(&mut self) {
        self.refs.clear();
        self.ranges.clear();
    }

    fn recompute(&mut self, formula: &Formula) {
        self.clear();

        // extend:
        {
            let mut stack = vec![formula];

            while let Some(formula) = stack.pop() {
                match *formula {
                    Formula::Number(_) => {}
                    Formula::Call(_, ref args) => {
                        for arg in args {
                            stack.push(arg);
                        }
                    }
                    Formula::Ref(v) => self.refs.push(v),
                    Formula::Range(range) => self.ranges.push(range),
                }
            }
        }

        // compress:
        self.ranges.sort_by_key(|range| (range.s, range.t));
        self.ranges.dedup();

        self.refs
            .retain(|&v| !self.ranges.iter().any(|r| r.contains(v)));

        self.refs.sort();
        self.refs.dedup();
    }
}

struct EvalFn<'a> {
    values: &'a GridArray<CellValue>,
    dirty: &'a GridArray<bool>,
}

impl<'a> EvalFn<'a> {
    fn compute(&self, formula: &Formula) -> CellValue {
        match formula {
            Formula::Number(s) => CellValue::Number(s.parse().unwrap()),

            Formula::Call(fn_kind, args) => match (fn_kind, args.as_slice()) {
                (Fn::Add, [l, r]) => number_binary(self.compute(l), self.compute(r), |l, r| l + r),
                (Fn::Subtract, [l, r]) => {
                    number_binary(self.compute(l), self.compute(r), |l, r| l - r)
                }
                (Fn::Multiply, [l, r]) => {
                    number_binary(self.compute(l), self.compute(r), |l, r| l * r)
                }
                (Fn::Divide, [l, r]) => match (self.compute(l), self.compute(r)) {
                    (CellValue::Number(l), CellValue::Number(r)) => {
                        if r.abs() > 1e-9 {
                            CellValue::Number(l / r)
                        } else {
                            CellValue::DividedByZero
                        }
                    }
                    _ => CellValue::Invalid,
                },
                (Fn::Modulo, [l, r]) => match (self.compute(l), self.compute(r)) {
                    (CellValue::Number(l), CellValue::Number(r)) => {
                        if r.abs() > 1e-9 {
                            CellValue::Number(l % r)
                        } else {
                            CellValue::DividedByZero
                        }
                    }
                    _ => CellValue::Invalid,
                },
                (Fn::Sum, [Formula::Range(range)]) => {
                    let mut sum = 0.0;
                    for p in range.iter_cells() {
                        if self.dirty[p] {
                            return CellValue::Recursive;
                        }

                        match self.values[p] {
                            CellValue::Null => continue,
                            CellValue::Number(n) => sum += n,
                            _ => {
                                #[cfg(test)]
                                eprintln!("invalid value in range {:?}", self.values[p]);
                                return CellValue::Invalid;
                            }
                        }
                    }
                    CellValue::Number(sum)
                }
                (Fn::Prod, [Formula::Range(range)]) => {
                    let mut prod = 1.0;
                    for p in range.iter_cells() {
                        if self.dirty[p] {
                            return CellValue::Recursive;
                        }

                        match self.values[p] {
                            CellValue::Null => continue,
                            CellValue::Number(n) => {
                                if n.abs() < 1e-9 {
                                    prod = 0.0;
                                    break;
                                }
                                prod *= n;
                            }
                            _ => {
                                #[cfg(test)]
                                eprintln!("invalid value in range {:?}", self.values[p]);
                                return CellValue::Invalid;
                            }
                        }
                    }
                    CellValue::Number(prod)
                }
                _ => {
                    #[cfg(test)]
                    eprintln!("invalid call {:?}", formula);
                    CellValue::Invalid
                }
            },
            &Formula::Ref(p) => {
                if self.dirty[p] {
                    return CellValue::Recursive;
                }
                self.values[p].clone()
            }
            Formula::Range(_) => {
                #[cfg(test)]
                eprintln!("invalid formula {formula:?}");
                CellValue::Invalid
            }
        }
    }
}

fn number_binary(
    l: CellValue,
    r: CellValue,
    combinator: impl std::ops::Fn(f64, f64) -> f64,
) -> CellValue {
    match (l, r) {
        (CellValue::Number(l), CellValue::Number(r)) => CellValue::Number(combinator(l, r)),
        _ => {
            #[cfg(test)]
            eprintln!("invalid pair: {l:?}, {r:?}");
            CellValue::Invalid
        }
    }
}

pub(crate) struct TableData {
    /// input[p] = (セルpに入力された値または数式)
    input: GridArray<CellInput>,
    /// values[p] = (セルpの数式を評価した値)
    values: GridArray<CellValue>,
    /// deps[p] = (セルpの数式が他のどのセルに対する参照を持つかを計算したもの)
    deps: GridArray<FormulaDeps>,
    /// dirty[p] = (true: 再評価が必要, false: 評価済み)
    ///
    /// `dirty[p] == true` であるセルの `values[p]` を参照してはいけない
    dirty: GridArray<bool>,
    /// back_deps[p] = (セルpを参照する数式を持つセルの集合)
    back_deps: GridArray<HashSet<Coord>>,
    /// 最後の更新より後に入力が変更されたセルの集合
    dirty_set: HashSet<Coord>,
    /// 最後の更新より後に値が変化したセルの集合
    modified_set: HashSet<Coord>,
    /// テーブルの大きさ。`size = (h, w)` はテーブルの行数がh、列数がwであることを表す
    size: Coord,
}

impl TableData {
    pub(crate) fn new(size: Coord) -> Self {
        Self {
            input: GridArray::new_with_value(CellInput::Null, size),
            values: GridArray::new_with_value(CellValue::Null, size),
            deps: GridArray::new(size),
            dirty: GridArray::new(size),
            back_deps: GridArray::new(size),
            dirty_set: HashSet::new(),
            modified_set: HashSet::new(),
            size,
        }
    }

    #[allow(unused)]
    pub(crate) fn input_at(&self, p: impl Into<Coord>) -> CellInput {
        let p = Into::<Coord>::into(p);
        self.input[p].clone()
    }

    #[allow(unused)]
    pub(crate) fn value_at(&self, p: impl Into<Coord>) -> CellValue {
        let p = Into::<Coord>::into(p);
        self.values[p]
    }

    pub(crate) fn set(&mut self, p: Coord, s: &str) {
        let input = match CellInput::parse(s) {
            Some(it) => it,
            None => {
                #[cfg(test)]
                eprintln!("input parse failed {s:?}");
                CellInput::Null
            }
        };

        self.input[p] = input;
        self.dirty_set.insert(p);
    }

    pub(crate) fn drain_changes<'a>(&'a mut self) -> impl IntoIterator<Item = Coord> + 'a {
        self.modified_set.drain()
    }

    /// 差分更新を行う
    pub(crate) fn update(&mut self) {
        // 手順:
        // 入力が変更されたセルの依存関係を再計算する
        //      そのセルが参照していた他のセルから、そのセル自身へのback_depを取り除く
        //      そのセルのdepsを更新する
        //      そのセルが参照している他のセルに、そのセル自身へのback_depを加える
        //
        // 次に、再評価が必要なセルの集合を用意する。
        //      初期値は入力が変更されたセルの集合に等しい
        //      なお再評価が必要な集合にセルを加えるのはセル1つにつき最大1回までに限る
        // 再評価が必要なセルの集合が空でなくなるまで、以下の繰り返し処理を行う
        // 再評価が必要なセルをdirty状態にする
        // 再評価が必要なセル同士の依存関係に従って、それらの更新順序を決定する
        //      参照されているセルを先に、参照しているセルを後に更新する
        //      (循環参照がある場合はどちらが先になってもよい)
        // その順番でセルの値を評価し、dirty状態をクリアする
        //      計算値が変化した場合、それを参照しているセルを再評価が必要なセルの集合に、次の繰り返しの際に加える
        // 最終的に集合に入っていてたセルはdirtyでない状態になる
        //
        // 備考:
        // 全体の最悪計算量はO((WH)^2)ぐらいある
        // 入力が変更されたセルが最大O(WH)、それらを推移的に参照しているセルを含めても最大O(WH)
        // セル1個あたりのback_depsの更新がO(WH)
        // セル1個あたりの評価時の計算量がO(WH) (sumの範囲が広いとき)
        // よくあるケースでは高速に動く
        //      入力が変更されたセルが1個だけで、それを推移的に参照しているセルが少ないとき

        #[cfg(test)]
        eprintln!("update {:?}", {
            let mut dirty_cells = self.dirty_set.iter().collect::<Vec<_>>();
            dirty_cells.sort();
            dirty_cells
        });

        let mut work_set = self.dirty_set.clone();
        let mut next_set: HashSet<Coord> = HashSet::new();
        let mut done: HashSet<Coord> = HashSet::new();

        // 依存関係の更新:
        {
            for &p in &self.dirty_set {
                #[cfg(test)]
                let mut old_refs = self.deps[p].iter_cells().collect::<HashSet<_>>();

                for q in self.deps[p].iter_cells() {
                    let removed = self.back_deps[q].remove(&p);
                    debug_assert!(removed);
                }

                match &self.input[p] {
                    CellInput::Formula(f) => {
                        self.deps[p].recompute(f);

                        for q in self.deps[p].iter_cells() {
                            self.back_deps[q].insert(p);
                        }

                        #[cfg(test)]
                        {
                            // 参照の差分を出力する
                            let mut new_refs = vec![];
                            for q in self.deps[p].iter_cells() {
                                if !old_refs.remove(&q) {
                                    new_refs.push(q);
                                }
                            }
                            let mut old_refs = old_refs.iter().collect::<Vec<_>>();
                            old_refs.sort();
                            if !(old_refs.is_empty() && new_refs.is_empty()) {
                                eprintln!("dep changed: {p:?} -> +{:?} -{:?}", new_refs, old_refs);
                            }
                        }
                    }
                    _ => {
                        self.deps[p].clear();

                        #[cfg(test)]
                        if !old_refs.is_empty() {
                            let mut old_refs = old_refs.iter().collect::<Vec<_>>();
                            old_refs.sort();
                            eprintln!("dep changed: {p:?} -> -{:?} (clear)", old_refs);
                        }
                    }
                }
            }
        }

        // 再評価:
        {
            // 再帰処理のスタック
            let mut stack: Vec<Coord> = vec![];

            // 再帰の状態を持つテーブル
            // (0: 訪問前, 1: 訪問中, 2: 訪問後)
            let mut state: GridArray<i32> = GridArray::new(self.size);

            while !work_set.is_empty() {
                debug_assert!(stack.is_empty());

                done.extend(&work_set);
                stack.extend(&work_set);

                for &p in &stack {
                    self.dirty[p] = true;
                }

                while let Some(p) = stack.pop() {
                    match state[p] {
                        0 => {
                            state[p] = 1;

                            // セル自身をスタックに積み直す
                            // (後で再訪し、帰りがけの処理を行う)
                            stack.push(p);

                            // 依存しているセルをスタックに積む
                            // (依存しているセルの状態が 0:訪問前 であるときだけスタックに積む
                            //  そうでないセルをスタックに積まない理由は以下の通り:
                            //  - 訪問中のときは、循環参照が起こっている。そのセルをスタックに積むと無限ループに陥る
                            //  - 訪問後のときは、スタックに積んでも何も起きない)

                            for q in self.deps[p].iter_cells() {
                                if work_set.contains(&q) && state[q] == 0 {
                                    stack.push(q);
                                }
                            }
                        }
                        1 => {
                            state[p] = 2;

                            debug_assert!(self.dirty[p]);
                            let old_value = self.values[p];

                            // セルの値を計算する
                            // (この時点で、セルが参照している他のセルの値はすべて計算済みのはず
                            //  そうでなければ循環参照が起こっている)
                            let value = match self.input[p] {
                                CellInput::Null => CellValue::Null,
                                CellInput::Number(value) => CellValue::Number(value),
                                CellInput::Formula(ref formula) => EvalFn {
                                    values: &self.values,
                                    dirty: &self.dirty,
                                }
                                .compute(formula),
                            };
                            self.values[p] = value;
                            self.dirty[p] = false;

                            if value != old_value {
                                self.modified_set.insert(p);

                                for &q in &self.back_deps[p] {
                                    if !done.contains(&q) {
                                        #[cfg(test)]
                                        eprintln!("propagate {p:?} -> {q:?}");
                                        next_set.insert(q);
                                    }
                                }
                            }

                            #[cfg(test)]
                            {
                                if value != old_value {
                                    eprintln!("eval {p:?} changed ({old_value:?} -> {value:?})");
                                } else {
                                    eprintln!("eval {p:?} unchanged ({value:?})");
                                }
                            }
                        }
                        _ => {}
                    }
                }

                std::mem::swap(&mut next_set, &mut work_set);
                next_set.clear();
            }
        }

        debug_assert!({
            let is_clean = self.dirty.iter().all(|&dirty| !dirty);
            is_clean
        });
        self.dirty_set.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_table((h, w): (usize, usize), cells: &[((usize, usize), &str)]) -> TableData {
        let mut table = TableData::new(Coord::from((h, w)));

        for &(p, s) in cells {
            table.set(p.into(), s);
        }

        table.update();
        table
    }

    // add関数が使えること
    #[test]
    fn test_add_fn() {
        let table = make_table(
            (3, 3),
            &[
                // C0 = A0 + B0
                ((0, 0), "2"),
                ((0, 1), "3"),
                ((0, 2), "=add(A0, B0)"),
            ],
        );
        let values = table.values;

        assert_eq!(values[0][0], CellValue::Number(2.0));
        assert_eq!(values[0][1], CellValue::Number(3.0));
        assert_eq!(values[0][2], CellValue::Number(5.0));

        // その他のセルはnullになっているはず
        assert_eq!(values[1][0], CellValue::Null);
        assert_eq!(values[2][2], CellValue::Null);
    }

    // 他のセルを参照できること。特に、順番が前にあるセルと後にあるセルの両方を参照できること
    #[test]
    fn test_refs() {
        let table = make_table(
            (3, 3),
            &[
                // A0 -> B0 (後ろのセルへの依存)
                ((0, 0), "=B0"),
                ((0, 1), "1"),
                // B1 -> A1 (前のセルへの依存)
                ((1, 0), "2"),
                ((1, 1), "=A1"),
                // B2 -> A2, C2 (複数のセルへの依存)
                ((2, 0), "3"),
                ((2, 1), "=add(A2, C2)"),
                ((2, 2), "4"),
            ],
        );
        let values = table.values;

        assert_eq!(values[0][0], CellValue::Number(1.0));
        assert_eq!(values[0][1], CellValue::Number(1.0));

        assert_eq!(values[1][0], CellValue::Number(2.0));
        assert_eq!(values[1][1], CellValue::Number(2.0));

        assert_eq!(values[2][1], CellValue::Number(3.0 + 4.0));
    }

    // 推移的に他のセルを参照できること
    #[test]
    fn test_transitive_refs() {
        let table = make_table(
            (3, 3),
            &[
                // A0 -> B0 -> C0
                ((0, 0), "=B0"),
                ((0, 1), "=C0"),
                ((0, 2), "3"),
            ],
        );
        let values = table.values;

        assert_eq!(values[0][0], CellValue::Number(3.0));
        assert_eq!(values[0][1], CellValue::Number(3.0));
        assert_eq!(values[0][2], CellValue::Number(3.0));
    }

    // 循環参照がエラーになること
    #[test]
    fn test_recurse() {
        let table = make_table(
            (3, 3),
            &[
                // A0 -> B0 -> C0 -> A0
                ((0, 0), "=B0"),
                ((0, 1), "=C0"),
                ((0, 2), "=A0"),
            ],
        );
        let values = table.values;

        assert_eq!(values[0][0], CellValue::Recursive);
        assert_eq!(values[0][1], CellValue::Recursive);
        assert_eq!(values[0][2], CellValue::Recursive);
    }

    // 組み込みの算術関数が使えること
    #[test]
    fn test_arithmetic_fn() {
        let table = make_table(
            (2, 7),
            &[
                ((0, 0), "13"),
                ((0, 1), "3"),
                // Ops
                ((0, 2), "=add(A0, B0)"),
                ((0, 3), "=sub(A0, B0)"),
                ((0, 4), "=mul(A0, B0)"),
                ((0, 5), "=div(A0, B0)"),
                ((0, 6), "=mod(A0, B0)"),
                // Div by zero
                ((1, 5), "=div(A0, 0)"),
                ((1, 6), "=mod(A0, 0)"),
            ],
        );
        let values = table.values;

        assert_eq!(values[0][0], CellValue::Number(13.0));
        assert_eq!(values[0][1], CellValue::Number(3.0));

        assert_eq!(values[0][2], CellValue::Number(13.0 + 3.0));
        assert_eq!(values[0][3], CellValue::Number(13.0 - 3.0));
        assert_eq!(values[0][4], CellValue::Number(13.0 * 3.0));
        assert_eq!(values[0][5], CellValue::Number(13.0 / 3.0));
        assert_eq!(values[0][6], CellValue::Number((13 % 3) as f64));

        // ゼロ除算エラーが起こること
        assert_eq!(values[1][5], CellValue::DividedByZero);
        assert_eq!(values[1][6], CellValue::DividedByZero);
    }

    // 組み込みの集約関数が使えること
    #[test]
    fn test_aggregation_fn() {
        let table = make_table(
            (3, 8),
            &[
                // A-H
                ((0, 0), "31"),
                ((0, 1), "41"),
                ((0, 2), "59"),
                ((0, 3), "26"),
                ((0, 4), "53"),
                ((0, 5), "58"),
                ((0, 6), "97"),
                ((0, 7), "0"),
                // sum
                ((1, 0), "=sum(A0:A0)"),
                ((1, 1), "=sum(A0:B0)"),
                ((1, 2), "=sum(A0:H0)"),
                // prod
                ((2, 0), "=prod(A0:A0)"),
                ((2, 1), "=prod(A0:B0)"),
                ((2, 2), "=prod(A0:H0)"),
            ],
        );
        let values = table.values;

        // sum:
        assert_eq!(values[1][0], CellValue::Number(31.0));
        assert_eq!(values[1][1], CellValue::Number(31.0 + 41.0));
        assert_eq!(
            values[1][2],
            CellValue::Number(31.0 + 41.0 + 59.0 + 26.0 + 53.0 + 58.0 + 97.0)
        );

        // prod:
        assert_eq!(values[2][0], CellValue::Number(31.0));
        assert_eq!(values[2][1], CellValue::Number(31.0 * 41.0));
        assert_eq!(values[2][2], CellValue::Number(0.0));
    }

    // 更新が伝播されること
    #[test]
    fn test_change_propagation() {
        let mut table = make_table(
            (4, 4),
            &[
                // B0 = A0 * 2
                ((0, 0), "1"),
                ((0, 1), "=mul(A0, 2)"),
                // C1 = A1 + B1
                // D1 = A1 + C1
                ((1, 0), "0"),
                ((1, 1), "0"),
                ((1, 2), "=add(A1, B1)"),
                ((1, 3), "=add(A1, C1)"),
            ],
        );
        assert_eq!(table.values[0][1], CellValue::Number(2.0));

        table.set((0, 0).into(), "2");
        table.update();
        assert_eq!(table.values[0][1], CellValue::Number(4.0));

        table.set((1, 0).into(), "3");
        table.set((1, 1).into(), "5");
        table.update();
        assert_eq!(table.values[1][2], CellValue::Number(3.0 + 5.0));
        assert_eq!(table.values[1][3], CellValue::Number(3.0 + 8.0));
    }

    // 更新が伝播されること
    #[test]
    fn test_formula_change() {
        let mut table = make_table(
            (1, 5),
            &[
                // D0 = A0 + B0
                // E0 = A0 + C0
                ((0, 0), "7"),
                ((0, 1), "11"),
                ((0, 2), "13"),
                ((0, 3), "=add(A0, B0)"),
                ((0, 4), "=add(A0, C0)"),
            ],
        );
        assert_eq!(table.values[0][3], CellValue::Number(18.0));

        // D0 = B0 * C0 (A0の参照がなくなり、C0の参照が増える)
        table.set((0, 3).into(), "=mul(A0, C0)");
        // 数式ではなくなる
        table.set((0, 4).into(), "42");
        table.update();

        assert_eq!(table.values[0][3], CellValue::Number(7.0 * 13.0));
        assert_eq!(table.values[0][4], CellValue::Number(42.0));

        // 増えた参照の変更を追跡できていることを確認する
        table.set((0, 0).into(), "17");
        table.update();

        assert_eq!(table.values[0][3], CellValue::Number(17.0 * 13.0));
    }
}
