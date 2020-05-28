use std::fmt::{self, Debug, Display, Formatter};
use std::rc::Rc;

#[derive(Clone)]
pub struct RcStr {
    full_text: Rc<String>,
    range: (usize, usize),
}

impl RcStr {
    pub fn new(full_text: Rc<String>, range: (usize, usize)) -> Self {
        Self { full_text, range }
    }

    pub fn as_str(&self) -> &str {
        let (start, end) = self.range;
        &self.full_text[start..end]
    }
}

impl Debug for RcStr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", self.as_str()))
    }
}

impl Display for RcStr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", self.as_str()))
    }
}
