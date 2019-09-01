/// `vi![x, y, ..]` は `vec![x.into(), y.into(), ..]` と同様
#[macro_export]
macro_rules! vi {
    ($($e:expr),*) => {{
        let mut v = vec![];
        $(v.push($e.into()))*;
        v
    }};
}

pub(crate) mod prefix_calc;
pub(crate) mod tokenize;

pub(crate) use super::*;
pub(crate) use tokenize::tokenize;
