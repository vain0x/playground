use std::fmt::Debug;

use crate::{coord::*, formula::*};

#[derive(Clone, PartialEq)]
enum CellValue {
    Null,
    Number(f64),

    // Bad
    Invalid,
    Recursive,
}

impl Debug for CellValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CellValue::Null => write!(f, ""),
            CellValue::Number(value) => write!(f, "{value:.2}"),
            CellValue::Invalid => write!(f, "#invalid"),
            CellValue::Recursive => write!(f, "#recursive"),
        }
    }
}

#[derive(Clone)]
enum CellInput {
    Null,
    Number(f64),
    Formula(Formula),
}

#[cfg(test)]
mod tests {
    use super::*;

    fn vec_list(formula: &Formula) -> Vec<GridVec> {
        match *formula {
            Formula::Number(_) => vec![],
            Formula::Call(_, ref args) => args.iter().flat_map(|arg| vec_list(arg)).collect(),
            Formula::Ref(v) => vec![v],
            Formula::Range(range) => {
                let mut v = vec![];
                for y in range.s.y..range.t.y {
                    for x in range.s.x..range.t.x {
                        v.push(GridVec::new(y, x));
                    }
                }
                v
            }
        }
    }

    struct EvalFn<'a> {
        display: &'a [Vec<CellValue>],
        done: &'a [Vec<bool>],
    }

    impl<'a> EvalFn<'a> {
        fn compute(&self, formula: &Formula) -> CellValue {
            match formula {
                Formula::Number(s) => CellValue::Number(s.parse().unwrap()),

                Formula::Call(fn_kind, args) => match (fn_kind, args.as_slice()) {
                    (Fn::Add, [l, r]) => match (self.compute(l), self.compute(r)) {
                        (CellValue::Number(l), CellValue::Number(r)) => CellValue::Number(l + r),
                        (l, r) => {
                            eprintln!("add error: {l:?}, {r:?}");
                            CellValue::Null
                        }
                    },
                    _ => CellValue::Invalid,
                },

                Formula::Ref(v) => {
                    let (y, x) = (v.y as usize, v.x as usize);
                    if !self.done[y][x] {
                        return CellValue::Recursive;
                    }
                    self.display[y][x].clone()
                }

                // Invalid
                Formula::Range(_) => CellValue::Null,
            }
        }
    }

    fn eval(formula: &Formula, display: &[Vec<CellValue>], done: &[Vec<bool>]) -> CellValue {
        EvalFn { display, done }.compute(formula)
    }

    fn update(table: &[Vec<CellInput>], display: &mut [Vec<CellValue>]) {
        let mut deps = vec![vec![vec![]; 8]; 8];

        for y in 0..table.len() {
            for x in 0..table[y].len() {
                match &table[y][x] {
                    CellInput::Formula(f) => {
                        let vs = vec_list(f);
                        if !vs.is_empty() {
                            eprintln!("{y},{x} ({f:?}) -> {vs:?}");
                        }
                        deps[y][x].extend(vs);
                    }
                    _ => {}
                }
            }
        }

        let mut pre = vec![vec![false; 8]; 8];
        let mut stack = vec![];
        for y in 0..table.len() {
            for x in 0..table[y].len() {
                for &u in deps[y][x].iter().rev() {
                    if !pre[u.y as usize][u.x as usize] {
                        pre[u.y as usize][u.x as usize] = true;
                        stack.push(u);
                    }
                }

                if !pre[y][x] {
                    pre[y][x] = true;
                    stack.push(GridVec::new(y as u32, x as u32));
                }
            }
        }

        let mut done = vec![vec![false; 8]; 8];
        stack.reverse();
        while let Some(pos) = stack.pop() {
            let y = pos.y as usize;
            let x = pos.x as usize;

            let value = match table[y][x] {
                CellInput::Null => CellValue::Null,
                CellInput::Number(value) => CellValue::Number(value),
                CellInput::Formula(ref formula) => eval(formula, &display, &done),
            };
            display[y][x] = value;
            done[pos.y as usize][pos.x as usize] = true;
        }
    }

    #[test]
    fn test_compute_formula() {
        let mut table = vec![vec![CellInput::Null; 8]; 8];
        let mut display = vec![vec![CellValue::Null; 8]; 8];

        // (0, 0) <- (0, 1)
        table[0][0] = CellInput::Number(2.0);
        table[0][1] = CellInput::Formula(Formula::Ref(GridVec::new(0, 0)));

        // (1, 1) <- (1, 0)
        table[1][1] = CellInput::Number(3.0);
        table[1][0] = CellInput::Formula(Formula::Ref(GridVec::new(1, 1)));

        // (2, 0) -> (2, 1) -> (2, 2) -> (2, 0) (recursive reference)
        table[2][0] = CellInput::Formula(Formula::Ref(GridVec::new(2, 1)));
        table[2][1] = CellInput::Formula(Formula::Ref(GridVec::new(2, 2)));
        table[2][2] = CellInput::Formula(Formula::Ref(GridVec::new(2, 0)));

        update(&table, &mut display);

        assert_eq!(display[0][0], CellValue::Number(2.0));
        assert_eq!(display[0][1], CellValue::Number(2.0));

        assert_eq!(display[1][0], CellValue::Number(3.0));
        assert_eq!(display[1][1], CellValue::Number(3.0));

        assert_eq!(display[2][0], CellValue::Recursive);
        assert_eq!(display[2][1], CellValue::Recursive);
        assert_eq!(display[2][2], CellValue::Recursive);
    }

    #[test]
    fn test_compute_add_fn() {
        let mut table = vec![vec![CellInput::Null; 8]; 8];
        let mut display = vec![vec![CellValue::Null; 8]; 8];

        table[0][0] = CellInput::Number(2.0);
        table[0][1] = CellInput::Number(3.0);
        table[0][2] = CellInput::Formula(Formula::parse("add(A0, B0)").unwrap());

        update(&table, &mut display);

        assert_eq!(display[0][0], CellValue::Number(2.0));
        assert_eq!(display[0][1], CellValue::Number(3.0));
        assert_eq!(display[0][2], CellValue::Number(5.0));
    }
}
