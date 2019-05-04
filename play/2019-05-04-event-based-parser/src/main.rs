mod event;
mod grammar;
mod lexer;
mod parser;
mod token_source;
mod tree_sink;

pub(crate) use crate::event::{Event, EventId};
pub(crate) use crate::parser::Parser;
pub(crate) use crate::token_source::TokenSource;
pub(crate) use crate::tree_sink::TreeSink;

#[derive(Clone, Debug)]
pub enum ParseError {
    Message(String),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SyntaxKind {
    Error,
    /// ノードではないことを表す印
    Tombstone,

    // トークンの種類
    /// コードの終端を表す、特殊なトークン
    EofToken,
    /// 空白を表すトークン
    WhitespaceToken,
    /// 整数トークン
    IntToken,
    /// '+' 記号のトークン
    PlusToken,
    /// '-' 記号のトークン
    MinusToken,

    // 式 (具象構文木のノード) の種類
    /// 根ノード
    RootExp,
    /// リテラル式 (`1` や `"hello"` など)
    LitExp,
    /// 二項演算の式
    BinExp,
}

impl SyntaxKind {
    pub(crate) fn is_trivia(self) -> bool {
        // or comment
        self == SyntaxKind::WhitespaceToken
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Token {
    kind: SyntaxKind,
    len: usize,
}

fn build(source: &str) {
    let source = source.to_string();

    let raw_tokens = lexer::tokenize(source.clone());
    eprintln!("Tokens = {:?}", raw_tokens);

    let token_source = TokenSource::new(source.clone(), &raw_tokens);
    let mut sink = TreeSink::new(source, raw_tokens);

    let events = parser::parse(token_source, grammar::parse_root);
    eprintln!("Events = {:?}", events);

    event::process(&mut sink, events);
    sink.finish();
}

fn main() {
    // build("3");
    // build("  3  ");
    build(" 1  + 2 - 3 ");
}
