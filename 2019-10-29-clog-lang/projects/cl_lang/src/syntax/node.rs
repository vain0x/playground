use super::*;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Node {
    kind: NodeKind,
    token: Option<Token>,
    children: Vec<Node>,
}

impl Node {
    pub(crate) fn new_node(kind: NodeKind, children: Vec<Node>) -> Self {
        Node {
            kind,
            token: None,
            children,
        }
    }

    pub(crate) fn new_leaf(kind: NodeKind) -> Self {
        Node::new_node(kind, vec![])
    }

    pub(crate) fn new_token(token: Option<Token>) -> Self {
        Node {
            kind: NodeKind::Token,
            token,
            children: vec![],
        }
    }

    pub(crate) fn new_miss() -> Self {
        Node::new_token(None)
    }

    pub(crate) fn kind(&self) -> NodeKind {
        self.kind
    }

    pub fn is<T: Ast>(&self) -> bool {
        T::is_base_of(self)
    }

    pub(crate) fn as_token(&self) -> Option<&Token> {
        self.token.as_ref()
    }

    pub(crate) fn children(&self) -> &[Node] {
        &self.children
    }

    pub(crate) fn child_token_iter<'a>(&'a self) -> impl Iterator<Item = &'a Token> + 'a {
        self.children().iter().filter_map(|node| node.as_token())
    }

    pub(crate) fn child_token_first<'a>(&'a self, pred: impl Fn(&Token) -> bool + 'a) -> Option<&'a Token> {
        self.child_token_iter().filter(|&token| pred(token)).next()
    }

    pub(crate) fn desc_token_iter<'a>(&'a self, pred: impl Fn(&Token) -> bool + 'a) -> Box<dyn Iterator<Item = &'a Token> + 'a> {
        let tokens = self.token.filter(pred).iter();
        let children = self.children().iter().flat_map(|node| node.desc_token_iter(pred));
        Box::new(tokens.chain(children))
    }

    pub(crate) fn desc_token_first<'a>(&'a self, pred: impl Fn(&Token) -> bool + 'a) -> Option<&'a Token> {
        self.desc_token_iter(pred).next()
    }

    // pub(crate) fn first_desc_token(&self, pred: impl Fn(&Token) -> bool) -> Option<&Token> {
    //     match self {
    //         Node::Token(token) => if pred(token) {
    //             Some(token)
    //         } else {
    //             None
    //         }
    //         Node::Expr { children, .. } => children.iter().filter_map(|node| node.find_token_opt(pred))
    //     }
    // }

    // pub(crate) fn first_child_node(&self, pred: impl Fn(&Token) -> bool) -> Option<&Node> {

    // }
}

impl From<Token> for Node {
    fn from(token: Token) -> Node {
        Node::new_token(Some(token))
    }
}

impl From<Option<Token>> for Node {
    fn from(token_opt: Option<Token>) -> Node {
        Node::new_token(token_opt)
    }
}

impl From<Option<Node>> for Node {
    fn from(node_opt: Option<Node>) -> Node {
        node_opt.unwrap_or(Node::new_miss())
    }
}
