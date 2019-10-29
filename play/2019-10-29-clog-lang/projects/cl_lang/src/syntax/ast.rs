use super::*;
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NodeKind {
    Token,
    Name,
    Atom,
    Block,
    Call,
    RuleDecl,
    RootDecl,
}

pub trait Ast {
    fn is_base_of(node: &Node) -> bool;

    fn parse(p: &mut Parser) -> Option<Node>;
}

#[derive(Clone, Debug)]
pub struct NameExpr;

impl NameExpr {
    fn ident_opt(node: &Node) -> Option<&Token> {
        assert!(node.is::<Self>());

        node.child_token_first(|t| t.kind() == TokenKind::Ident)
    }
}

#[derive(Clone, Debug)]
pub struct AtomExpr;

impl AtomExpr {
    fn atom_opt(node: &Node) -> Option<&Token> {
        assert!(node.is::<Self>());

        node.child_token_first(|t| t.kind() == TokenKind::Atom)
    }
}

#[derive(Clone, Debug)]
pub struct BlockExpr;

#[derive(Clone, Debug)]
pub struct CallExpr;

#[derive(Clone, Debug)]
pub struct RuleDecl;

#[derive(Clone, Debug)]
pub struct RootDecl;

fn parse_atomic(p: &mut Parser) -> Option<Node> {
    NameExpr::parse(p)
        .or_else(|| AtomExpr::parse(p))
        .or_else(|| BlockExpr::parse(p))
}

fn parse_term(p: &mut Parser) -> Option<Node> {
    CallExpr::parse(p)
}

impl Ast for NameExpr {
    fn is_base_of(node: &Node) -> bool {
        node.kind() == NodeKind::Name
    }

    fn parse(p: &mut Parser) -> Option<Node> {
        if !p.at(TokenKind::Ident) {
            return None;
        }

        let token = p.bump();
        Some(Node::new_node(NodeKind::Name, vec![token.into()]))
    }
}

impl Ast for AtomExpr {
    fn is_base_of(node: &Node) -> bool {
        node.kind() == NodeKind::Atom
    }

    fn parse(p: &mut Parser) -> Option<Node> {
        if !p.at(TokenKind::Atom) {
            return None;
        }

        let token = p.bump();
        Some(Node::new_node(NodeKind::Atom, vec![token.into()]))
    }
}

impl Ast for BlockExpr {
    fn is_base_of(node: &Node) -> bool {
        node.kind() == NodeKind::Block
    }

    fn parse(p: &mut Parser) -> Option<Node> {
        if !p.at(TokenKind::BraceL) {
            return None;
        }

        let brace_l = p.bump();

        let brace_r = if p.at(TokenKind::BraceR) {
            Some(p.bump())
        } else {
            None
        };

        Some(Node::new_node(
            NodeKind::Block,
            vec![brace_l.into(), brace_r.into()],
        ))
    }
}

impl Ast for CallExpr {
    fn is_base_of(node: &Node) -> bool {
        node.kind() == NodeKind::Call
    }

    fn parse(p: &mut Parser) -> Option<Node> {
        let cal = parse_atomic(p)?;

        if !p.at(TokenKind::ParenL) {
            return Some(cal);
        }

        let arg = parse_atomic(p)?;

        if !p.at(TokenKind::ParenR) {
            p.error("Missing ')'");
        }

        Some(Node::new_node(NodeKind::Call, vec![cal, arg]))
    }
}

impl Ast for RuleDecl {
    fn is_base_of(node: &Node) -> bool {
        node.kind() == NodeKind::RuleDecl
    }

    fn parse(p: &mut Parser) -> Option<Node> {
        if !p.at(TokenKind::Rule) {
            return None;
        }

        let keyword = p.bump();
        let name = NameExpr::parse(p);

        let paren_l = if !p.at(TokenKind::ParenL) {
            p.error("Missing '('");
            None
        } else {
            Some(p.bump())
        };
        let param = parse_term(p);
        let paren_r = if !p.at(TokenKind::ParenR) {
            p.error("Missing ')'");
            None
        } else {
            Some(p.bump())
        };

        let body = BlockExpr::parse(p);

        Some(Node::new_node(
            NodeKind::RuleDecl,
            vec![
                keyword.into(),
                name.into(),
                paren_l.into(),
                param.into(),
                paren_r.into(),
                body.into(),
            ],
        ))
    }
}

impl Ast for RootDecl {
    fn is_base_of(node: &Node) -> bool {
        node.kind() == NodeKind::RootDecl
    }

    fn parse(p: &mut Parser) -> Option<Node> {
        let mut children = vec![];

        while let Some(query_decl) = RuleDecl::parse(p) {
            children.push(query_decl);
        }

        if !p.at(TokenKind::Eof) {
            p.error("Expected EOF");
        }

        Some(Node::new_node(NodeKind::RootDecl, children))
    }
}
