use super::*;

impl Printer {
    pub fn new(config: PrinterConfig) -> Self {
        let mut docs = vec![
            Doc::Nil,
            Doc::Line,
        ];
        Printer {
            config,
            docs,
            nil_doc_id: 0,
            line_doc_id: 1,
            root_doc_id: 0,
        }
    }
}
