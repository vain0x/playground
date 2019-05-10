mod config;
mod doc;
mod printer;

type TextId = usize;
type DocId = usize;

#[derive(Clone, Copy, Debug)]
pub struct Doc(DocId);

/// ドキュメント。
/// レイアウトの集合を表す。
#[derive(Clone, Debug)]
enum DocKind {
    /// 空文字列
    Nil,
    /// 改行
    Line,
    /// 分解不可能な文字列
    Text(String),
    /// 連結 (<>)
    Cat(Doc, Doc),
    /// 字下げ
    Nest { depth: usize, content: Doc },
    /// (<|>)
    /// 2つのドキュメントが表現するレイアウトの和集合を表す。
    /// 不変条件: first, second は flatten したらどちらも同じレイアウトになること
    ///
    Union(Doc, Doc),
    /// ドキュメントを1行にする。つまり改行とインデントを ' ' に潰す。
    Flatten(Doc),
}

#[derive(Clone, Debug)]
pub struct PrintConfig {
    indent_size: usize,
    width: usize,
}

pub struct Printer {
    docs: Vec<DocKind>,
    nil_doc: Doc,
    line_doc: Doc,
}
