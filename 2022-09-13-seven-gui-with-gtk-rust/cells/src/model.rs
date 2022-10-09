use crate::{coord::*, formula::*};
use std::fmt::Debug;

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

impl FormulaDeps {
    #[cfg(test)]
    fn is_empty(&self) -> bool {
        self.refs.is_empty() && self.ranges.is_empty()
    }

    fn recompute(&mut self, formula: &Formula) {
        // clear:
        self.refs.clear();
        self.ranges.clear();

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
                _ => CellValue::Invalid,
            },

            Formula::Ref(v) => {
                let (y, x) = v.pair();
                if self.dirty[y][x] {
                    return CellValue::Recursive;
                }
                self.values[y][x].clone()
            }

            // Invalid
            Formula::Range(_) => CellValue::Null,
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
    input: Vec<Vec<CellInput>>,
    values: Vec<Vec<CellValue>>,
    deps: Vec<Vec<FormulaDeps>>,
    dirty: Vec<Vec<bool>>,
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
    }

    fn recompute(&mut self) {
        init_deps(&mut self.deps, &self.input, self.size);
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
    }
}

fn init_deps(deps: &mut [Vec<FormulaDeps>], input: &[Vec<CellInput>], size: GridVec) {
    let (h, w) = size.pair();

    for y in 0..h {
        for x in 0..w {
            let f = match &input[y][x] {
                CellInput::Formula(f) => f,
                _ => continue,
            };

            deps[y][x].recompute(f);

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
}
