use super::*;
use std::fmt::{self, Debug, Formatter};
use std::path::PathBuf;
use std::rc::Rc;

#[derive(Clone)]
pub struct SourceFile {
    path: Rc<PathBuf>,
    text: Rc<String>,
}

#[derive(Clone)]
pub struct SourceLocation {
    file: SourceFile,
    pos: TextPos,
}

impl SourceFile {
    pub(crate) fn new(path: Rc<PathBuf>, text: Rc<String>) -> Self {
        Self { path, text }
    }

    pub(crate) fn path(&self) -> Rc<PathBuf> {
        Rc::clone(&self.path)
    }

    pub(crate) fn text(&self) -> Rc<String> {
        Rc::clone(&self.text)
    }

    pub fn loc(&self, pos: TextPos) -> SourceLocation {
        SourceLocation::new(self.clone(), pos)
    }
}

impl SourceLocation {
    pub(crate) fn new(file: SourceFile, pos: TextPos) -> Self {
        Self {
            file,
            pos,
        }
    }
}

impl Debug for SourceFile {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", self.path))
    }
}

impl Debug for SourceLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}:{}", self.file.path(), self.pos))
    }
}
