use builders::{ComputedDoc, Doc};

pub fn is_empty(doc: &Doc) -> bool {
    match doc {
        Doc::Source(ref source) => source.is_empty(),
        Doc::Computed(_) => false,
    }
}

pub fn will_break(doc: &Doc) -> bool {
    find_in_doc(
        doc,
        |doc| {
            match doc {
                Doc::Computed(ComputedDoc::Group { breaks, .. }) if *breaks => Some(true),
                // Doc::Computed(ComputedDoc::Line(LineKind::Hard)) => true,
                Doc::Computed(ComputedDoc::BreakParent) => Some(true),
                _ => None,
            }
        },
        false,
    )
}

pub fn is_line_next(doc: &Doc) -> bool {
    find_in_doc(
        doc,
        |doc| match doc {
            Doc::Source(_) => Some(false),
            Doc::Computed(ComputedDoc::Line(_)) => Some(true),
            Doc::Computed(_) => None,
        },
        false,
    )
}

fn find_in_doc<T, F>(_doc: &Doc, _f: F, _default_value: T) -> T
where
    F: Fn(&Doc) -> Option<T>,
{
    unimplemented!()
}

pub fn traverse_doc<FEnter, FExit>(
    _doc: &Doc,
    _on_enter: FEnter,
    _on_exit: FExit,
    _should_traverse_conditional_groups: bool,
) where
    FEnter: Fn(&Doc),
    FExit: Fn(&Doc),
{
    unimplemented!()
}

pub fn map_doc<F>(_doc: &Doc, _f: &F) -> Doc
where
    F: Fn(&Doc) -> Doc,
{
    unimplemented!()
}

// FIXME: port
