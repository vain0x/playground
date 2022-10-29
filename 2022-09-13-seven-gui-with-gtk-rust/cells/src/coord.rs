use std::fmt::Debug;

/// グリッド上のベクトル。座標や相対的な範囲を表す
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Coord {
    pub(crate) y: u32,
    pub(crate) x: u32,
}

impl Coord {
    pub(crate) const ZERO: Coord = Coord { y: 0, x: 0 };

    pub(crate) fn new(y: u32, x: u32) -> Self {
        Coord { y, x }
    }

    pub(crate) fn pair(self) -> (usize, usize) {
        (self.y as usize, self.x as usize)
    }

    /// Get whether `p` is a point in an area with size equal to `self`, exclusively.
    pub(crate) fn contains(&self, p: Coord) -> bool {
        p.y < self.y && p.x < self.x
    }

    /// Get whether `p` is a point in an area with size equal to `self`, inclusively.
    pub(crate) fn contains_inclusive(self, p: Coord) -> bool {
        p.y <= self.y && p.x <= self.x
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

    fn add(self, rhs: Self) -> Self {
        Coord {
            y: self.y + rhs.y,
            x: self.x + rhs.x,
        }
    }
}

impl std::ops::Sub for Coord {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
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
pub(crate) struct CoordRange {
    pub(crate) s: Coord,
    pub(crate) t: Coord,
}

impl CoordRange {
    pub(crate) fn new(s: Coord, t: Coord) -> Self {
        CoordRange { s, t }
    }

    pub(crate) fn contains(self, p: Coord) -> bool {
        self.s <= p && p <= self.t
    }

    pub(crate) fn iter_cells(self: CoordRange) -> impl Iterator<Item = Coord> {
        let range = self;
        (range.s.y..range.t.y)
            .flat_map(move |y| (range.s.x..range.t.x).map(move |x| Coord::new(y, x)))
    }
}

impl From<std::ops::Range<Coord>> for CoordRange {
    fn from(range: std::ops::Range<Coord>) -> Self {
        CoordRange {
            s: range.start,
            t: range.end,
        }
    }
}

impl From<std::ops::RangeTo<Coord>> for CoordRange {
    fn from(range: std::ops::RangeTo<Coord>) -> Self {
        CoordRange {
            s: Coord::ZERO,
            t: range.end,
        }
    }
}

impl Debug for CoordRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}-{:?}", self.s, self.t)
    }
}

/// グリッドの座標系を備える2次元配列
#[derive(Clone, Default)]
pub(crate) struct GridArray<T> {
    inner: Box<[T]>,
    size: Coord,
}

#[allow(unused)]
impl<T> GridArray<T> {
    pub(crate) fn new(size: Coord) -> Self
    where
        T: Clone + Default,
    {
        Self::new_with_value(T::default(), size)
    }

    pub(crate) fn new_with_value(value: T, size: Coord) -> Self
    where
        T: Clone,
    {
        let (h, w) = size.pair();
        let n = h.checked_mul(w).expect("size");

        Self {
            inner: vec![value; n].into_boxed_slice(),
            size,
        }
    }

    pub(crate) const fn size(&self) -> Coord {
        self.size
    }

    fn columns_len(&self) -> usize {
        self.size.x as usize
    }

    fn rows_len(&self) -> usize {
        self.size.y as usize
    }

    pub(crate) fn get(&self, index: Coord) -> Option<&T> {
        if index.y < self.size.y && index.x < self.size.x {
            let w = self.size.x as usize;
            let (y, x) = index.pair();
            Some(unsafe { self.inner.get_unchecked(y * w + x) })
        } else {
            None
        }
    }

    pub(crate) fn get_mut(&mut self, index: Coord) -> Option<&mut T> {
        if index.y < self.size.y && index.x < self.size.x {
            let w = self.size.x as usize;
            let (y, x) = index.pair();
            Some(unsafe { self.inner.get_unchecked_mut(y * w + x) })
        } else {
            None
        }
    }

    /// 要素の座標を昇順で列挙するイテレータ
    pub(crate) fn coords(&self) -> impl Iterator<Item = Coord> {
        CoordRange::new(Coord::default(), self.size).iter_cells()
    }

    /// 要素への参照を昇順で列挙するイテレータ
    pub(crate) fn iter(&self) -> std::slice::Iter<'_, T> {
        self.inner.iter()
    }

    /// 要素への排他参照を昇順で列挙するイテレータ
    pub(crate) fn iter_mut(&mut self) -> std::slice::IterMut<'_, T> {
        self.inner.iter_mut()
    }

    /// 要素のインデックスと参照を昇順で列挙するイテレータ
    pub(crate) fn enumerate<'a>(&'a self) -> impl Iterator<Item = (Coord, &'a T)> + 'a {
        self.coords().zip(self.iter())
    }
}

// Indexing to an item.
impl<T> std::ops::Index<Coord> for GridArray<T> {
    type Output = T;

    fn index(&self, index: Coord) -> &T {
        assert!(index.y < self.size.y);
        assert!(index.x < self.size.x);
        let w = self.size.x as usize;
        let (y, x) = index.pair();
        unsafe { self.inner.get_unchecked(y * w + x) }
    }
}

// Indexing to an item.
impl<T> std::ops::IndexMut<Coord> for GridArray<T> {
    fn index_mut(&mut self, index: Coord) -> &mut T {
        assert!(index.y < self.size.y);
        assert!(index.x < self.size.x);
        let w = self.size.x as usize;
        let (y, x) = index.pair();
        unsafe { self.inner.get_unchecked_mut(y * w + x) }
    }
}

// Indexing to a row.
impl<T> std::ops::Index<usize> for GridArray<T> {
    type Output = [T];

    fn index(&self, index: usize) -> &[T] {
        assert!(index < (self.size.y as usize));
        let w = self.size.x as usize;
        &self.inner[w * index..w * (index + 1)]
    }
}

// Indexing to a row.
impl<T> std::ops::IndexMut<usize> for GridArray<T> {
    fn index_mut(&mut self, index: usize) -> &mut [T] {
        assert!(index < (self.size.y as usize));
        let w = self.size.x as usize;
        &mut self.inner[w * index..w * (index + 1)]
    }
}
