use builders::Doc;

fn flatten_doc(doc: &Doc) -> &Doc {
    // FIXME: port
    doc
}

fn print_doc(doc: &Doc) -> String {
    format!("{:?}", doc)
}

// FIXME: use Debug trait
pub fn print_doc_to_debug(doc: &Doc) -> String {
    print_doc(flatten_doc(doc))
}
