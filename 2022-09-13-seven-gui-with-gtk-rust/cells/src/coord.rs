use std::fmt::Debug;

/// グリッド上のベクトル。座標や相対的な範囲を表す
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Coord {
    pub(crate) y: u32,
    pub(crate) x: u32,
}

impl Coord {
    pub(crate) fn new(y: u32, x: u32) -> Self {
        Coord { y, x }
    }

    pub(crate) fn pair(self) -> (usize, usize) {
        (self.y as usize, self.x as usize)
    }
}

impl From<(usize, usize)> for Coord {
    fn from((y, x): (usize, usize)) -> Self {
        Coord {
            y: y as u32,
            x: x as u32,
        }
    }
}

impl std::ops::Add for Coord {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Coord {
            y: self.y + rhs.y,
            x: self.x + rhs.x,
        }
    }
}

impl std::ops::Sub for Coord {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Coord {
            y: self.y - rhs.y,
            x: self.x - rhs.x,
        }
    }
}

impl Debug for Coord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.y, self.x)
    }
}

/// グリッド上の範囲
#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct GridRange {
    pub(crate) s: Coord,
    pub(crate) t: Coord,
}

impl GridRange {
    pub(crate) fn new(s: Coord, t: Coord) -> Self {
        GridRange { s, t }
    }

    pub(crate) fn contains(self, v: Coord) -> bool {
        self.s <= v && v <= self.t
    }
}

impl Debug for GridRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}-{:?}", self.s, self.t)
    }
}
