use crate::{coord::*, formula::*};
use std::{collections::HashSet, fmt::Debug};

#[allow(unused)]
#[derive(Clone, Copy, PartialEq)]
enum CellValue {
    Null,
    Number(f64),

    // Bad
    Invalid,
    Recursive,
    DividedByZero,
}

impl Debug for CellValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CellValue::Null => write!(f, ""),
            CellValue::Number(value) => write!(f, "{value:.2}"),
            CellValue::Invalid => write!(f, "#invalid"),
            CellValue::Recursive => write!(f, "#recursive"),
            CellValue::DividedByZero => write!(f, "#DIV/0!"),
        }
    }
}

#[allow(unused)]
#[derive(Clone)]
enum CellInput {
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
    refs: Vec<GridVec>,
    ranges: Vec<GridRange>,
}

impl GridRange {
    fn iter_cells(self: GridRange) -> impl Iterator<Item = GridVec> {
        let range = self;
        (range.s.y..range.t.y)
            .flat_map(move |y| (range.s.x..range.t.x).map(move |x| GridVec::new(y, x)))
    }
}

impl FormulaDeps {
    fn is_empty(&self) -> bool {
        self.refs.is_empty() && self.ranges.is_empty()
    }

    fn iter_cells<'a>(&'a self) -> impl Iterator<Item = GridVec> + 'a {
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
    values: &'a [Vec<CellValue>],
    dirty: &'a [Vec<bool>],
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
                    for y in range.s.y..range.t.y {
                        for x in range.s.x..range.t.x {
                            let (y, x) = GridVec::new(y, x).pair();
                            if self.dirty[y][x] {
                                return CellValue::Recursive;
                            }

                            match self.values[y][x] {
                                CellValue::Null => continue,
                                CellValue::Number(n) => sum += n,
                                _ => {
                                    #[cfg(test)]
                                    eprintln!("invalid value in range {:?}", self.values[y][x]);
                                    return CellValue::Invalid;
                                }
                            }
                        }
                    }
                    CellValue::Number(sum)
                }
                (Fn::Prod, [Formula::Range(range)]) => {
                    let mut prod = 1.0;
                    for y in range.s.y..range.t.y {
                        for x in range.s.x..range.t.x {
                            let (y, x) = GridVec::new(y, x).pair();
                            if self.dirty[y][x] {
                                return CellValue::Recursive;
                            }

                            match self.values[y][x] {
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
                                    eprintln!("invalid value in range {:?}", self.values[y][x]);
                                    return CellValue::Invalid;
                                }
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
            Formula::Ref(v) => {
                let (y, x) = v.pair();
                if self.dirty[y][x] {
                    return CellValue::Recursive;
                }
                self.values[y][x].clone()
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

struct TableData {
    /// input[v] = (セルvに入力された値または数式)
    input: Vec<Vec<CellInput>>,
    /// values[v] = (セルvの数式を評価した値)
    values: Vec<Vec<CellValue>>,
    /// deps[v] = (セルvの数式が他のどのセルに対する参照を持つかを計算したもの)
    deps: Vec<Vec<FormulaDeps>>,
    /// dirty[v] = (true: 再評価が必要, false: 評価済み)
    ///
    /// `dirty[v] == true` であるセルの `values[v]` を参照してはいけない
    dirty: Vec<Vec<bool>>,
    /// back_deps[v] = (vを参照する数式を持つセルの集合)
    back_deps: Vec<Vec<HashSet<GridVec>>>,
    /// 最後の更新より後に入力が変更されたセルの集合
    dirty_set: HashSet<GridVec>,
    /// テーブルの大きさ。`size = (h, w)` はテーブルの行数がh、列数がwであることを表す
    size: GridVec,
}

#[allow(unused)]
impl TableData {
    fn new(size: GridVec) -> Self {
        let (h, w) = size.pair();

        Self {
            input: vec![vec![CellInput::Null; w]; h],
            values: vec![vec![CellValue::Null; w]; h],
            deps: vec![vec![FormulaDeps::default(); w]; h],
            dirty: vec![vec![true; w]; h],
            back_deps: vec![vec![HashSet::new(); w]; h],
            dirty_set: HashSet::new(),
            size,
        }
    }

    fn input_at(&self, v: impl Into<GridVec>) -> CellInput {
        let (y, x) = Into::<GridVec>::into(v).pair();
        self.input[y][x].clone()
    }

    fn value_at(&self, v: impl Into<GridVec>) -> CellValue {
        let (y, x) = Into::<GridVec>::into(v).pair();
        self.values[y][x]
    }

    fn set(&mut self, v: GridVec, s: &str) {
        let input = match CellInput::parse(s) {
            Some(it) => it,
            None => {
                #[cfg(test)]
                eprintln!("input parse failed {s:?}");
                CellInput::Null
            }
        };

        let (y, x) = v.pair();
        self.input[y][x] = input;
        self.dirty_set.insert(v);
    }

    /// 差分更新を行う
    fn update(&mut self) {
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
        let mut next_set: HashSet<GridVec> = HashSet::new();
        let mut done: HashSet<GridVec> = HashSet::new();

        let (h, w) = self.size.pair();

        // 依存関係の更新:
        {
            for &v in &self.dirty_set {
                let (y, x) = v.pair();

                #[cfg(test)]
                let mut old_refs = self.deps[y][x].iter_cells().collect::<HashSet<_>>();

                for w in self.deps[y][x].iter_cells() {
                    let (wy, wx) = w.pair();
                    let removed = self.back_deps[wy][wx].remove(&v);
                    debug_assert!(removed);
                }

                match &self.input[y][x] {
                    CellInput::Formula(f) => {
                        self.deps[y][x].recompute(f);

                        for w in self.deps[y][x].iter_cells() {
                            let (wy, wx) = w.pair();
                            self.back_deps[wy][wx].insert(v);
                        }

                        #[cfg(test)]
                        {
                            // 参照の差分を出力する
                            let mut new_refs = vec![];
                            for v in self.deps[y][x].iter_cells() {
                                if !old_refs.remove(&v) {
                                    new_refs.push(v);
                                }
                            }
                            let mut old_refs = old_refs.iter().collect::<Vec<_>>();
                            old_refs.sort();
                            if !(old_refs.is_empty() && new_refs.is_empty()) {
                                eprintln!("dep changed: {v:?} -> +{:?} -{:?}", new_refs, old_refs);
                            }
                        }
                    }
                    _ => {
                        self.deps[y][x].clear();

                        #[cfg(test)]
                        if !old_refs.is_empty() {
                            let mut old_refs = old_refs.iter().collect::<Vec<_>>();
                            old_refs.sort();
                            eprintln!("dep changed: {v:?} -> -{:?} (clear)", old_refs);
                        }
                    }
                }
            }
        }

        // 再評価:
        {
            // 再帰処理のスタック
            let mut stack: Vec<GridVec> = vec![];

            // 再帰の状態を持つテーブル
            // (0: 訪問前, 1: 訪問中, 2: 訪問後)
            let mut state = vec![vec![0; w]; h];

            while !work_set.is_empty() {
                debug_assert!(stack.is_empty());

                done.extend(&work_set);
                stack.extend(&work_set);

                for &v in &stack {
                    let (y, x) = v.pair();
                    self.dirty[y][x] = true;
                }

                while let Some(v) = stack.pop() {
                    let (y, x) = v.pair();
                    match state[y][x] {
                        0 => {
                            state[y][x] = 1;

                            let v = GridVec::from((y, x));
                            stack.push(v);

                            for v in self.deps[y][x].iter_cells() {
                                let (y, x) = v.pair();
                                if work_set.contains(&v) && state[y][x] == 0 {
                                    stack.push(v);
                                }
                            }
                        }
                        1 => {
                            state[y][x] = 2;

                            debug_assert!(self.dirty[y][x]);
                            let old_value = self.values[y][x];

                            let (y, x) = v.pair();
                            let value = match self.input[y][x] {
                                CellInput::Null => CellValue::Null,
                                CellInput::Number(value) => CellValue::Number(value),
                                CellInput::Formula(ref formula) => EvalFn {
                                    values: &self.values,
                                    dirty: &self.dirty,
                                }
                                .compute(formula),
                            };
                            self.values[y][x] = value;
                            self.dirty[y][x] = false;

                            if value != old_value {
                                for &w in &self.back_deps[y][x] {
                                    if !done.contains(&w) {
                                        #[cfg(test)]
                                        eprintln!("propagate {v:?} -> {w:?}");
                                        next_set.insert(w);
                                    }
                                }
                            }

                            #[cfg(test)]
                            {
                                if value != old_value {
                                    eprintln!("eval {v:?} changed ({old_value:?} -> {value:?})");
                                } else {
                                    eprintln!("eval {v:?} unchanged ({value:?})");
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
            let is_clean = self.dirty.iter().all(|row| row.iter().all(|&dirty| !dirty));
            is_clean
        });
        self.dirty_set.clear();
    }

    /// 全体を更新する
    fn recompute(&mut self) {
        let (h, w) = self.size.pair();
        for y in 0..h {
            for x in 0..w {
                self.back_deps[y][x].clear();
            }
        }

        init_deps(&mut self.deps, &mut self.back_deps, &self.input, self.size);

        init_values(
            &mut self.values,
            &mut self.dirty,
            &self.input,
            &self.deps,
            self.size,
        );

        debug_assert!({
            let is_clean = self.dirty.iter().all(|row| row.iter().all(|&dirty| !dirty));
            is_clean
        });
        self.dirty_set.clear();
    }
}

fn init_deps(
    deps: &mut [Vec<FormulaDeps>],
    back_deps: &mut [Vec<HashSet<GridVec>>],
    input: &[Vec<CellInput>],
    size: GridVec,
) {
    let (h, w) = size.pair();

    for y in 0..h {
        for x in 0..w {
            let f = match &input[y][x] {
                CellInput::Formula(f) => f,
                _ => continue,
            };

            deps[y][x].recompute(f);

            // 逆方向の依存関係を記録する
            {
                let d = &deps[y][x];
                if !d.is_empty() {
                    let u = GridVec::from((y, x));

                    for w in deps[y][x].iter_cells() {
                        let (wy, wx) = w.pair();
                        back_deps[wy][wx].insert(u);
                    }
                }
            }

            #[cfg(test)]
            {
                let d = &deps[y][x];
                if !d.is_empty() {
                    let u = GridVec::from((y, x));
                    eprintln!("dep: {u:?} ({f:?}) -> {:?} {:?}", d.refs, d.ranges);
                }
            }
        }
    }
}

fn init_values(
    values: &mut [Vec<CellValue>],
    dirty: &mut [Vec<bool>],
    input: &[Vec<CellInput>],
    deps: &[Vec<FormulaDeps>],
    size: GridVec,
) {
    let (h, w) = size.pair();

    for row in dirty.iter_mut() {
        row.fill(true);
    }

    // 再帰処理のスタック
    let mut stack = vec![];

    // 再帰の状態を持つテーブル
    // (0: 訪問前, 1: 訪問中, 2: 訪問後)
    // (典型的にはWhite-Gray-Blackの3色を使う)
    let mut state = vec![vec![0; w]; h];

    // すべてのセルを逆順にスタックに積む
    // (すべてのセルを前方から順に訪問するため)
    for y in (0..h).rev() {
        for x in (0..w).rev() {
            stack.push(GridVec::from((y, x)));
        }
    }

    while let Some(v) = stack.pop() {
        let (y, x) = v.pair();
        match state[y][x] {
            0 => {
                state[y][x] = 1;

                // セル自身をスタックに積み直す
                // (後で再訪し、帰りがけの処理を行う)
                let v = GridVec::from((y, x));
                stack.push(v);

                // 依存しているセルをスタックに積む
                // (依存しているセルの状態が 0:訪問前 であるときだけスタックに積む
                //  そうでないセルをスタックに積まない理由は以下の通り:
                //  - 訪問中のときは、循環参照が起こっている。そのセルをスタックに積むと無限ループに陥る
                //  - 訪問後のときは、スタックに積んでも何も起きない)
                for &v in deps[y][x].refs.iter().rev() {
                    let (y, x) = v.pair();
                    if state[y][x] == 0 {
                        stack.push(v);
                    }
                }

                for &range in deps[y][x].ranges.iter().rev() {
                    for y in (range.s.y..range.t.y).rev() {
                        for x in (range.s.x..range.t.x).rev() {
                            let v = GridVec::new(y, x);
                            let (y, x) = v.pair();
                            if state[y][x] == 0 {
                                stack.push(v);
                            }
                        }
                    }
                }
            }
            1 => {
                state[y][x] = 2;

                // セルの値を計算する
                // (この時点で、セルが参照している他のセルの値はすべて計算済みのはず
                //  そうでなければ循環参照が起こっている)
                let (y, x) = v.pair();
                let value = match input[y][x] {
                    CellInput::Null => CellValue::Null,
                    CellInput::Number(value) => CellValue::Number(value),
                    CellInput::Formula(ref formula) => EvalFn { values, dirty }.compute(formula),
                };
                values[y][x] = value;
                dirty[y][x] = false;
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_table((h, w): (usize, usize), cells: &[((usize, usize), &str)]) -> TableData {
        let mut table = TableData::new(GridVec::from((h, w)));

        for &(v, s) in cells {
            table.set(v.into(), s);
        }

        table.recompute();
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
