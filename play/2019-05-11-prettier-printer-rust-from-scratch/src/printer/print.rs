use super::*;

type IndentDocId = usize;

/// 連結リストの先頭への参照のようなもの
type IndentDocList = Option<IndentDocId>;

type EmitId = usize;

/// `Emit` からなる連結リストの先頭への参照のようなもの
type EmitList = Option<EmitId>;

/// 字下げ付きのドキュメント
#[derive(Clone, Debug)]
struct IndentDoc {
    doc: Doc,
    depth: usize,
}

/// 構築されるテキストの構成要素
#[derive(Clone, Debug)]
enum Emit {
    Space,
    Text(String),
    Line(usize),
}

struct PrintContext<'a> {
    p: &'a Printer,

    /// ドキュメントの幅の最大値
    /// なるべく1行の長さがこれを超えないレイアウトを選ぶ
    /// (バイト数単位。Unicode 文字が含まれているケースは考慮していない。)
    width: usize,

    /// `IndentDoc` からなる連結リストのセルを配置する領域
    docs: Vec<(IndentDoc, IndentDocList)>,

    /// `Emit` からなる連結リストのセルを配置する領域
    emits: Vec<(Emit, EmitList)>,
}

const DO_FLATTEN: bool = true;
const NO_FLATTEN: bool = false;

impl PrintContext<'_> {
    /// ドキュメントリストの先頭要素の詳細と、tail を取得する。
    fn uncons_doc(
        &self,
        indent_doc_list: IndentDocList,
    ) -> Option<(&DocKind, usize, IndentDocList)> {
        match indent_doc_list {
            None => None,
            Some(indent_doc_id) => {
                let &(ref indent_doc, tail) = &self.docs[indent_doc_id];
                eprintln!(
                    "doc={} kind={:?} tail={:?}",
                    indent_doc.doc.0,
                    self.p.kind(indent_doc.doc),
                    tail
                );
                Some((self.p.kind(indent_doc.doc), indent_doc.depth, tail))
            }
        }
    }

    fn cons_doc(&mut self, doc: Doc, depth: usize, tail: IndentDocList) -> IndentDocId {
        let id = self.docs.len();
        self.docs.push((IndentDoc { doc, depth }, tail));
        id
    }

    fn cons_emit(&mut self, kind: Emit, tail: EmitList) -> EmitId {
        let id = self.emits.len();
        self.emits.push((kind, tail));
        id
    }

    // FIXME: use iterator?
    fn emits_to_vec(&self, mut emits: EmitList) -> Vec<&Emit> {
        let mut buf = vec![];

        while let Some(emit_id) = emits {
            let &(ref kind, next) = &self.emits[emit_id];
            buf.push(kind);
            emits = next;
        }

        buf.reverse();
        buf
    }

    /// `Emit` のリストが表現する文字列を構築する。
    /// `emits` は逆順に構築されているので注意。
    fn print_emits(&mut self, mut emits: EmitList, out: &mut String) {
        for emit in self.emits_to_vec(emits) {
            match emit {
                Emit::Space => out.push(' '),
                Emit::Text(text) => out.push_str(text),
                &Emit::Line(depth) => {
                    out.push('\n');
                    for _ in 0..depth {
                        out.push(' ');
                    }
                }
            }
        }
    }

    /// ドキュメントの最良のレイアウトを計算する。
    /// `column` : いまの行にすでに書き込まれている文字数 (バイト単位)
    /// `flatten` : すべてを1行に収めるレイアウトを選ぶモード
    fn be(
        &mut self,
        column: usize,
        flatten: bool,
        docs: Option<usize>,
        emits: EmitList,
    ) -> EmitList {
        let (kind, depth, docs) = match self.uncons_doc(docs) {
            None => return emits,
            Some(x) => x,
        };

        match kind {
            DocKind::Nil => self.be(column, flatten, docs, emits),
            DocKind::Line => {
                let emits = if flatten {
                    self.cons_emit(Emit::Space, emits)
                } else {
                    self.cons_emit(Emit::Line(depth), emits)
                };
                self.be(depth, flatten, docs, Some(emits))
            }
            &DocKind::Cat(first, second) => {
                let second = self.cons_doc(second, depth, docs);
                let first = self.cons_doc(first, depth, Some(second));
                self.be(column, flatten, Some(first), emits)
            }
            &DocKind::Nest {
                depth: shift,
                content,
            } => {
                let head = self.cons_doc(content, depth + shift, docs);
                self.be(column, flatten, Some(head), emits)
            }
            DocKind::Text(text) => {
                let text = text.to_string();
                let text_len = text.len();
                let emits = self.cons_emit(Emit::Text(text), emits);
                let column = column + text_len;
                self.be(column, flatten, docs, Some(emits))
            }
            &DocKind::Union(first, second) => {
                if flatten {
                    // flatten first == flatten second なので first が最良
                    let docs = self.cons_doc(first, depth, docs);
                    return self.be(column, flatten, Some(docs), emits);
                }

                // 1つ目のドキュメントの最良のレイアウトの最初の行が、現在の行に収まるなら、それが最良。(Union の不変条件より)
                let first_docs = self.cons_doc(first, depth, docs);
                let first_emits = self.be(column, NO_FLATTEN, Some(first_docs), None);
                if self.fits(column, &self.emits_to_vec(first_emits)) {
                    return self.be(column, NO_FLATTEN, Some(first_docs), emits);
                }

                let second_docs = self.cons_doc(second, depth, docs);
                self.be(column, NO_FLATTEN, Some(second_docs), emits)
            }
            &DocKind::Flatten(content) => {
                let docs = self.cons_doc(content, depth, docs);
                self.be(column, DO_FLATTEN, Some(docs), emits)
            }
        }
    }

    fn fits(&self, mut column: usize, emits: &[&Emit]) -> bool {
        for emit in emits {
            if column >= self.width {
                return false;
            }
            match emit {
                Emit::Space => {
                    column += 1;
                }
                Emit::Text(text) => {
                    column += text.len();
                }
                Emit::Line(..) => {
                    return true;
                }
            }
        }
        true
    }

    /// ドキュメントの最良のレイアウトを計算する。
    fn best(&mut self, column: usize, doc: Doc) -> EmitList {
        let docs = self.cons_doc(doc, 0, None);
        self.be(column, NO_FLATTEN, Some(docs), None)
    }
}

impl Printer {
    pub fn print(&self, doc: Doc, config: PrintConfig) -> String {
        eprintln!("{:?}", doc);

        let mut context = PrintContext {
            p: self,
            width: config.width,
            docs: vec![],
            emits: vec![],
        };
        let emits = context.best(0, doc);

        let mut out = String::with_capacity(1024);
        context.print_emits(emits, &mut out);
        out
    }
}
