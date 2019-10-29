pub mod ast;
pub mod node;
pub(crate) mod parser;
pub mod rc_str;
pub mod source;
pub(crate) mod text_pos;
pub mod token;
pub(crate) mod token_stream;

pub use ast::*;
pub use node::*;
pub(crate) use parser::Parser;
pub use rc_str::*;
pub use source::*;
pub use text_pos::*;
pub use token::*;
pub(crate) use token_stream::TokenStream;

pub(crate) mod functions {
    use super::*;

    pub(crate) fn parse(file: SourceFile) -> Node {
        let mut p = Parser::new(file);
        RootDecl::parse(&mut p).unwrap()
    }
}
