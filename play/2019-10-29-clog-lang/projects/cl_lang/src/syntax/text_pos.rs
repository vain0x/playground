use std::cmp::min;
use std::fmt::{self, Debug, Display, Formatter};
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct TextPos {
    row: usize,
    col: usize,
}

pub(crate) struct TextPosCursor {
    text: Rc<String>,
    current: usize,
    row: usize,
    col: usize,
}

impl TextPos {
    pub fn new(row: usize, col: usize) -> Self {
        Self {
            row,
            col,
        }
    }
}

impl Debug for TextPos {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        <Self as Display>::fmt(self, f)
    }
}

impl Display for TextPos {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.row + 1, self.col + 1))
    }
}

impl TextPosCursor {
    pub(crate) fn new(text: Rc<String>) -> Self {
        Self {
            text,
            current: 0,
            row: 0,
            col: 0,
        }
    }

    pub(crate) fn current(&self) -> usize {
        self.current
    }

    pub(crate) fn pos(&self) -> TextPos {
        TextPos::new(self.row, self.col)
    }

    pub(crate) fn advance(&mut self, distance: usize) {
        let start = self.current;
        let end = min(self.text.len(), start + distance);

        for b in self.text[start..end].bytes() {
            if b == b'\n' {
                self.row += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }
        }

        self.current = end;
    }
}
