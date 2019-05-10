use super::*;

#[derive(Clone, Copy, Debug)]
struct IndentDoc(usize, Doc);

struct PrintContext<'a> {
    p: &'a Printer,
    out: String,
    width: usize,
    column: usize,
    candidates: Vec<IndentDoc>,
}

impl PrintContext<'_> {
    fn go(&mut self) {
        loop {
            match self
                .candidates
                .pop()
                .map(|IndentDoc(i, doc)| (i, self.p.kind(doc)))
            {
                None => break,
                Some((_, DocKind::Nil)) => {}
                Some((i, DocKind::Cat(first, second))) => {
                    self.candidates.push(IndentDoc(i, *second));
                    self.candidates.push(IndentDoc(i, *first));
                }
                Some((i, DocKind::Nest { depth, content })) => {
                    self.candidates.push(IndentDoc(i + depth, *content));
                }
                Some((i, DocKind::Text(text))) => {
                    self.column += text.len();
                    self.out += &text;
                }
                Some((i, DocKind::Line)) => {
                    self.out.push('\n');
                    for _ in 0..i {
                        self.out.push(' ');
                    }
                    self.column = i;
                }
                Some((i, DocKind::Union(..))) | Some((i, DocKind::Flatten(..))) => unimplemented!(),
            }
        }
    }

    fn print(&mut self, doc: Doc) {
        self.candidates.push(IndentDoc(0, doc));
        self.go();
    }
}

impl Printer {
    pub fn print(&self, doc: Doc, config: PrintConfig) -> String {
        let mut context = PrintContext {
            p: self,
            out: String::with_capacity(1024),
            width: config.width,
            column: 0,
            candidates: vec![],
        };
        context.print(doc);
        context.out
    }
}
