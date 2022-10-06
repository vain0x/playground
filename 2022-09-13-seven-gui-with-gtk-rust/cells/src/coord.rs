use std::fmt::Debug;

/// グリッド上のベクトル
#[derive(Clone, Copy, Default, PartialEq, PartialOrd)]
pub(crate) struct GridVec {
    pub(crate) y: u32,
    pub(crate) x: u32,
}

impl GridVec {
    pub(crate) fn new(y: u32, x: u32) -> Self {
        GridVec { y, x }
    }
}

impl std::ops::Add for GridVec {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        GridVec {
            y: self.y + rhs.y,
            x: self.x + rhs.x,
        }
    }
}

impl std::ops::Sub for GridVec {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        GridVec {
            y: self.y - rhs.y,
            x: self.x - rhs.x,
        }
    }
}

impl Debug for GridVec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.y, self.x)
    }
}

/// グリッド上の範囲
#[derive(Clone, Copy, Default)]
pub(crate) struct GridRange {
    pub(crate) s: GridVec,
    pub(crate) t: GridVec,
}

impl GridRange {
    pub(crate) fn new(s: GridVec, t: GridVec) -> Self {
        GridRange { s, t }
    }
}

impl Debug for GridRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}-{:?}", self.s, self.t)
    }
}
