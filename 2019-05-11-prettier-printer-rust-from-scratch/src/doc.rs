use super::*;

impl Doc {
    pub fn nil(p: &mut Printer) -> Doc {
        p.nil()
    }

    pub fn line(p: &mut Printer) -> Doc {
        p.line()
    }

    pub fn text(s: String, p: &mut Printer) -> Doc {
        let text_id = p.add_text(s);
        p.add_doc(DocKind::Text(text_id))
    }

    pub fn concat(self, other: Doc, p: &mut Printer) -> Doc {
        p.add_doc(DocKind::Cat(self, other))
    }

    pub fn nest(self, depth: usize, p: &mut Printer) -> Doc {
        p.add_doc(DocKind::Nest {
            depth,
            content: self,
        })
    }

    pub(crate) fn union(self, other: Doc, p: &mut Printer) -> Doc {
        p.add_doc(DocKind::Union(self, other))
    }

    pub(crate) fn flatten(self, p: &mut Printer) -> Doc {
        p.add_doc(DocKind::Flatten(self))
    }

    pub fn group(self, p: &mut Printer) -> Doc {
        let first = self.flatten(p);
        first.union(self, p)
    }
}
