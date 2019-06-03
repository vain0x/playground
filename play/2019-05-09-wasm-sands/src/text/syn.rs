use super::parser::{do_parse, Event};
use super::*;

const EMPTY_SYN_IDS: &[SynId] = &[];

impl<'a> SynTree<'a> {
    fn new(text: &'a str) -> Self {
        SynTree {
            text,
            nodes: vec![],
            errors: vec![],
        }
    }

    fn add_error(&mut self, msg: String, range: (usize, usize)) {
        self.errors.push((range, msg));
    }

    pub(crate) fn errors(&self) -> &[((usize, usize), String)] {
        &self.errors
    }

    fn new_node(&mut self, syn: Syn) -> SynId {
        let node = self.nodes.len();
        self.nodes.push(syn);
        node
    }

    fn add_child(&mut self, parent: SynId, child: SynId) {
        match &mut self.nodes[parent] {
            Syn::Token(..) => unreachable!(),
            Syn::Node {
                ref mut children, ..
            } => {
                children.push(child);
            }
        }
    }

    fn set_len(&mut self, node: SynId, len: usize) {
        match &mut self.nodes[node] {
            Syn::Token(..) => unreachable!(),
            Syn::Node {
                len: ref mut slot, ..
            } => {
                *slot = len;
            }
        }
    }

    fn recompute_len(&mut self, node: SynId) {
        fn go(st: &mut SynTree<'_>, node: SynId, sum: &mut usize) {
            let children = match &mut st.nodes[node] {
                &mut Syn::Token(Token { len, .. }) => {
                    *sum += len;
                    return;
                }
                &mut Syn::Node { ref children, .. } => {
                    children.into_iter().cloned().collect::<Vec<_>>()
                }
            };

            let mut subsum = 0;
            for child in children {
                go(st, child, &mut subsum);
            }
            st.set_len(node, subsum);
            *sum += subsum;
        }
    }

    pub(crate) fn kind(&self, syn_id: SynId) -> SynKind {
        match &self.nodes[syn_id] {
            Syn::Token(token) => SynKind::Token(token.kind),
            Syn::Node { kind, .. } => *kind,
        }
    }

    pub(crate) fn cast<T: Ast>(&self, syn_id: SynId) -> Option<T> {
        T::cast(syn_id, self)
    }

    pub(crate) fn child_ids(&self, syn_id: SynId) -> &[SynId] {
        match &self.nodes[syn_id] {
            Syn::Token(..) => EMPTY_SYN_IDS,
            Syn::Node { children, .. } => children.as_slice(),
        }
    }

    pub(crate) fn cast_children<T: Ast>(&self, parent: SynId) -> Vec<T> {
        self.child_ids(parent)
            .into_iter()
            .filter_map(|&child| T::cast(child, self))
            .collect()
    }

    pub(crate) fn cast_child<T: Ast>(&self, parent: SynId) -> Option<T> {
        self.child_ids(parent)
            .into_iter()
            .filter_map(|&child| T::cast(child, self))
            .next()
    }

    pub(crate) fn root_id(&self) -> SynId {
        0
    }

    pub(crate) fn debug(&self) {
        fn go(st: &SynTree<'_>, node: SynId, indent: &str, pos: &mut usize) {
            match &st.nodes[node] {
                Syn::Token(token) => {
                    let l = *pos;
                    *pos += token.len;
                    eprintln!("{}{:?}({}..{})", indent, token.kind, l, *pos);
                }
                Syn::Node { kind, children, .. } => {
                    eprintln!("{}<{:?}>", indent, kind);
                    {
                        let indent = format!("{}  ", indent);
                        for &child in children {
                            go(st, child, &indent, pos);
                        }
                    }
                    eprintln!("{}</{:?}>", indent, kind);
                }
            }
        }

        let mut pos = 0;
        go(self, self.root_id(), "", &mut pos);
    }
}

pub(crate) fn parse(text: &str) -> SynTree<'_> {
    let tokens = tokenize::tokenize(text);

    let mut nontrivial_tokens = Vec::with_capacity(tokens.len());
    let mut spans = Vec::with_capacity(tokens.len());
    let mut rev = Vec::with_capacity(tokens.len());
    let mut r = 0;
    for i in 0..tokens.len() {
        let l = r;
        r += tokens[i].len;

        if tokens[i].kind != TokenKind::Whitespace {
            nontrivial_tokens.push(tokens[i].clone());
            spans.push((l, r));
            rev.push(i);
        }
    }

    let mut st = SynTree::new(text);

    let mut indent = String::new();
    let mut stack = vec![];
    let mut last = 0;
    let mut t = 0;
    let mut pos = 0;
    let mut h = |event: Event| match event {
        Event::Error(msg) => {
            st.add_error(msg, (pos, pos + 1)); // FIXME: how does it extent?
        }
        Event::Start { kind } => {
            // eprintln!("{}<{:?}>", indent, kind);
            indent.push(' ');

            let node = st.new_node(Syn::Node {
                kind,
                children: vec![],
                len: 0,
            });
            stack.push(node);
        }
        Event::Finish => {
            let node = stack.pop().unwrap();
            indent.pop();
            // eprintln!("{}</{:?}>", indent, st.kind(node));

            if let Some(&parent) = stack.last() {
                st.add_child(parent, node);
            }
        }
        Event::Token => {
            let i = rev[t];

            while last <= i {
                let token = tokens[last].clone();
                let token_len = token.len;
                let node = st.new_node(Syn::Token(token));

                if let Some(&parent) = stack.last() {
                    st.add_child(parent, node);
                }

                pos += token_len;
                last += 1;
            }

            // eprintln!("{}TOKEN({:?})", indent, tokens[i].kind);
            t += 1;
        }
    };

    do_parse(
        text,
        &nontrivial_tokens,
        &spans,
        grammar::parse_root,
        &mut h,
    );

    st
}
