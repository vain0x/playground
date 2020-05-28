#[derive(Clone, Copy)]
pub(crate) enum DfsColor {
    White,
    Gray,
    Black,
}

impl DfsColor {
    pub(crate) fn mark_as_white(&mut self) {
        *self = DfsColor::White;
    }

    pub(crate) fn mark_as_gray(&mut self) -> bool {
        match *self {
            DfsColor::Gray | DfsColor::Black => false,
            DfsColor::White => {
                *self = DfsColor::Gray;
                true
            }
        }
    }

    pub(crate) fn mark_as_black(&mut self) -> bool {
        match *self {
            DfsColor::White | DfsColor::Black => false,
            DfsColor::Gray => {
                *self = DfsColor::Black;
                true
            }
        }
    }
}
