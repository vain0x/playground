use lsp_types::*;

const DIAGNOSTIC_SOURCE: &str = "Example LSP";

fn msg_to_diagnostic(start: (usize, usize), end: (usize, usize)) -> Diagnostic {
    let (ly, lx) = start;
    let (ry, rx) = end;

    Diagnostic {
        severity: Some(DiagnosticSeverity::Warning),
        range: Range {
            start: Position {
                line: ly as u64,
                character: lx as u64,
            },
            end: Position {
                line: ry as u64,
                character: rx as u64,
            },
        },
        message: "Upper case".to_string(),
        source: Some(DIAGNOSTIC_SOURCE.to_string()),
        ..Diagnostic::default()
    }
}

pub(crate) fn diagnostics(text: &str) -> Vec<Diagnostic> {
    // Make a warning for each upper-case words.
    let mut diagnostics = vec![];

    let mut row = 0;
    let mut col = 0;
    let mut i = 0;

    while i < text.len() {
        while i < text.len() && !text.as_bytes()[i].is_ascii_uppercase() {
            let c = text.as_bytes()[i];
            if c == b'\n' {
                row += 1;
                col = 0;
            } else {
                col += 1;
            }
            i += 1;
        }

        let start = (row, col);
        while i < text.len() && text.as_bytes()[i].is_ascii_uppercase() {
            let c = text.as_bytes()[i];
            if c == b'\n' {
                row += 1;
                col = 0;
            } else {
                col += 1;
            }
            i += 1;
        }
        let end = (row, col);

        if start < end {
            diagnostics.push(msg_to_diagnostic(start, end));
        }
    }

    diagnostics
}
