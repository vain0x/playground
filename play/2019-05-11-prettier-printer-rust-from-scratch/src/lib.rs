mod config;
mod pretty;
mod printer;

type TextId = usize;
type DocId = usize;

#[derive(Clone, Debug)]
pub(crate) enum Doc {
    Nil,
    Text(TextId),
    Cat(DocId, DocId),
    Line,
    Nest {
        depth: usize,
        content: DocId,
    },
}

#[derive(Clone, Debug)]
pub struct PrinterConfig {
    indent_size: usize,
    width: usize,
}

pub struct Printer {
    config: PrinterConfig,
    docs: Vec<Doc>,
    nil_doc_id: usize,
    line_doc_id: usize,
    root_doc_id: usize,
}
