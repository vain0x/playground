use super::*;

pub(crate) mod layout;
pub(crate) mod print;

impl Printer {
    pub fn new() -> Self {
        let mut docs = vec![DocKind::Nil, DocKind::Line];
        Printer {
            docs,
            nil_doc: Doc(0),
            line_doc: Doc(1),
        }
    }

    pub(crate) fn add_text(&mut self, text: String) -> String {
        text
    }

    pub(crate) fn nil(&self) -> Doc {
        self.nil_doc
    }

    pub(crate) fn line(&self) -> Doc {
        self.line_doc
    }

    pub(crate) fn add_doc(&mut self, doc: DocKind) -> Doc {
        let doc_id = self.docs.len();
        self.docs.push(doc);
        Doc(doc_id)
    }
}
