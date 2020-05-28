use std;

#[derive(Clone, Debug)]
pub enum Doc {
    Source(String),
    Computed(ComputedDoc),
}

#[derive(Clone, Debug)]
pub enum ComputedDoc {
    Concat {
        parts: Vec<Doc>,
    },
    Indent {
        contents: Box<Doc>,
    },
    Align {
        contents: Box<Doc>,
        n: AlignSize,
    },
    ReseltAlign {
        contents: Box<Doc>,
        n: AlignSize,
    },
    Group {
        contents: Box<Doc>,
        breaks: bool,
        expanded_states: Option<Vec<Doc>>,
    },
    Fill(Box<Fill>),
    IfBreak {
        break_contents: Option<Box<Doc>>,
        flat_contents: Option<Box<Doc>>,
    },
    LineSuffix {
        contents: Box<Doc>,
    },
    LineSuffixBoundary,
    BreakParent,
    Line(LineKind),
    Cursor,
}

#[derive(Clone, Debug)]
pub struct Fill {
    pub first: Doc,
    pub whitespace: Doc,
    pub second: Doc,
}

#[derive(Clone, Copy, Debug)]
pub enum AlignSize {
    Number(usize),
    NegativeInfinity,
}

#[derive(Clone, Copy, Debug)]
pub enum LineKind {
    Space,
    Soft,
    Hard,
    Literal,
}

impl LineKind {
    pub fn is_hard(self) -> bool {
        match self {
            LineKind::Hard | LineKind::Literal => true,
            LineKind::Space | LineKind::Soft => false,
        }
    }

    pub fn is_soft(self) -> bool {
        match self {
            LineKind::Soft => true,
            LineKind::Space | LineKind::Hard | LineKind::Literal => false,
        }
    }

    pub fn is_literal(self) -> bool {
        match self {
            LineKind::Literal => true,
            LineKind::Space | LineKind::Soft | LineKind::Hard => false,
        }
    }
}

pub struct GroupSetting {
    should_break: Option<bool>,
    expanded_states: Option<Vec<Doc>>,
}

impl std::default::Default for GroupSetting {
    fn default() -> GroupSetting {
        GroupSetting {
            should_break: None,
            expanded_states: None,
        }
    }
}

// unnecessary in rust
pub fn doc(value: Doc) -> Doc {
    value
}

// FIXME: rename later
fn com(doc: ComputedDoc) -> Doc {
    Doc::Computed(doc)
}

pub fn concat(parts: Vec<Doc>) -> Doc {
    com(ComputedDoc::Concat { parts })
}

pub fn indent(contents: Doc) -> Doc {
    com(ComputedDoc::Indent {
        contents: Box::new(contents),
    })
}

pub fn align(n: AlignSize, contents: Doc) -> Doc {
    com(ComputedDoc::Align {
        contents: Box::new(contents),
        n,
    })
}

pub fn group_ex(contents: Doc, setting: GroupSetting) -> Doc {
    com(ComputedDoc::Group {
        contents: Box::new(contents),
        breaks: setting.should_break.unwrap_or(false),
        expanded_states: setting.expanded_states,
    })
}

pub fn group(contents: Doc) -> Doc {
    group_ex(contents, GroupSetting::default())
}

pub fn conditional_group(_options: Vec<Doc>, _setting: Option<GroupSetting>) -> Doc {
    unimplemented!()
}

pub fn fill(parts: Vec<Doc>) -> Doc {
    fn go<T: Iterator<Item = Doc>>(mut iter: T) -> Doc {
        let first = match iter.next() {
            None => panic!("Doc can't be empty."),
            Some(x) => x,
        };

        let whitespace = match iter.next() {
            None => return first,
            Some(x) => x,
        };

        let second = go(iter);

        let fill = Fill {
            first,
            whitespace,
            second,
        };
        Doc::Computed(ComputedDoc::Fill(Box::new(fill)))
    }
    go(parts.into_iter().fuse())
}

pub fn if_break(break_contents: Option<Doc>, flat_contents: Option<Doc>) -> Doc {
    com(ComputedDoc::IfBreak {
        break_contents: break_contents.map(|x| Box::new(x)),
        flat_contents: flat_contents.map(|x| Box::new(x)),
    })
}

pub fn line_suffix(contents: Doc) -> Doc {
    com(ComputedDoc::LineSuffix {
        contents: Box::new(contents),
    })
}

pub const LINE_SUFFIX_BOUNDARY: Doc = Doc::Computed(ComputedDoc::LineSuffixBoundary);
pub const BREAK_PARENT: Doc = Doc::Computed(ComputedDoc::BreakParent);

pub const LINE: Doc = Doc::Computed(ComputedDoc::Line(LineKind::Space));

pub const SOFT_LINE: Doc = Doc::Computed(ComputedDoc::Line(LineKind::Soft));

// FIXME: HARD_LINE, LITERAL_LINE

pub const CURSOR: Doc = Doc::Computed(ComputedDoc::Cursor);

pub fn join(separator: Doc, parts: Vec<Doc>) -> Doc {
    concat(
        parts
            .into_iter()
            .enumerate()
            .flat_map(|(i, item)| {
                let first = i == 0;
                // FIXME: unnecessary allocation
                if first {
                    vec![item]
                } else {
                    vec![separator.clone(), item]
                }
            })
            .collect::<Vec<_>>(),
    )
}

pub fn add_alignment_to_doc(contents: Doc, size: usize, tab_width: usize) -> Doc {
    let mut aligned = contents;
    if size > 0 {
        // Use indent to add (hard or soft) tabs as possible.
        for _ in 0..(size / tab_width) {
            aligned = indent(aligned)
        }

        // Align by adding some spaces.
        aligned = align(AlignSize::Number(size % tab_width), aligned);

        // FIXME: not understanding
        // size is absolute from 0 and not relative to the current
        // indentation, so we use -Infinity to reset the indentation to 0
        aligned = align(AlignSize::NegativeInfinity, aligned);
    }
    aligned
}
