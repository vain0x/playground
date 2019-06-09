use super::*;

#[derive(Clone, Debug)]
pub(crate) struct Name {
    syn_id: SynId,
}

#[derive(Clone, Debug)]
pub(crate) struct Op {
    syn_id: SynId,
}

#[derive(Clone, Debug)]
pub(crate) struct Instr {
    syn_id: SynId,
}

#[derive(Clone, Debug)]
pub(crate) struct Val {
    syn_id: SynId,
}

#[derive(Clone, Debug)]
pub(crate) struct ResultTy {
    syn_id: SynId,
}

#[derive(Clone, Debug)]
pub(crate) struct ModuleDecl {
    syn_id: SynId,
}

#[derive(Clone, Debug)]
pub(crate) struct FuncDecl {
    syn_id: SynId,
}

#[derive(Clone, Debug)]
pub(crate) struct ExportDecl {
    syn_id: SynId,
}

#[derive(Clone, Debug)]
pub(crate) struct ExportDesc {
    syn_id: SynId,
}

impl Ast for ModuleDecl {
    fn syn_id(&self) -> SynId {
        self.syn_id
    }

    fn cast(syn_id: SynId, st: &SynTree<'_>) -> Option<Self> {
        if st.kind(syn_id) == SynKind::ModuleDecl {
            Some(ModuleDecl { syn_id })
        } else {
            None
        }
    }
}

impl Ast for FuncDecl {
    fn syn_id(&self) -> SynId {
        self.syn_id
    }

    fn cast(syn_id: SynId, st: &SynTree<'_>) -> Option<Self> {
        if st.kind(syn_id) == SynKind::FuncDecl {
            Some(FuncDecl { syn_id })
        } else {
            None
        }
    }
}

impl Ast for Name {
    fn syn_id(&self) -> SynId {
        self.syn_id
    }

    fn cast(syn_id: SynId, st: &SynTree<'_>) -> Option<Self> {
        if st.kind(syn_id) == SynKind::Token(TokenKind::Ident) {
            Some(Name { syn_id })
        } else {
            None
        }
    }
}

impl Ast for ResultTy {
    fn syn_id(&self) -> SynId {
        self.syn_id
    }

    fn cast(syn_id: SynId, st: &SynTree<'_>) -> Option<Self> {
        if st.kind(syn_id) == SynKind::ResultTy {
            Some(ResultTy { syn_id })
        } else {
            None
        }
    }
}

impl Ast for Op {
    fn syn_id(&self) -> SynId {
        self.syn_id
    }

    fn cast(syn_id: SynId, st: &SynTree<'_>) -> Option<Self> {
        if st.kind(syn_id) == SynKind::Op {
            Some(Op { syn_id })
        } else {
            None
        }
    }
}

impl Ast for Instr {
    fn syn_id(&self) -> SynId {
        self.syn_id
    }

    fn cast(syn_id: SynId, st: &SynTree<'_>) -> Option<Self> {
        if st.kind(syn_id) == SynKind::Instr {
            Some(Instr { syn_id })
        } else {
            None
        }
    }
}

impl Ast for Val {
    fn syn_id(&self) -> SynId {
        self.syn_id
    }

    fn cast(syn_id: SynId, st: &SynTree<'_>) -> Option<Self> {
        if st.kind(syn_id) == SynKind::Val {
            Some(Val { syn_id })
        } else {
            None
        }
    }
}

impl Ast for ExportDecl {
    fn syn_id(&self) -> SynId {
        self.syn_id
    }

    fn cast(syn_id: SynId, st: &SynTree<'_>) -> Option<Self> {
        if st.kind(syn_id) == SynKind::ExportDecl {
            Some(ExportDecl { syn_id })
        } else {
            None
        }
    }
}

impl Ast for ExportDesc {
    fn syn_id(&self) -> SynId {
        self.syn_id
    }

    fn cast(syn_id: SynId, st: &SynTree<'_>) -> Option<Self> {
        if st.kind(syn_id) == SynKind::ExportDesc {
            Some(ExportDesc { syn_id })
        } else {
            None
        }
    }
}

impl ResultTy {
    pub(crate) fn is_i32(&self, st: &SynTree<'_>) -> bool {
        st.find_first_token_by_kind(self.syn_id, TokenKind::Keyword(Keyword::I32))
            .is_some()
    }
}

impl Instr {
    pub(crate) fn op(&self, st: &SynTree<'_>) -> Option<Op> {
        st.cast_child(self.syn_id)
    }

    pub(crate) fn val(&self, st: &SynTree<'_>) -> Option<Val> {
        st.cast_child(self.syn_id)
    }
}

impl Op {
    pub(crate) fn pair(&self, st: &SynTree<'_>) -> Option<(Keyword, Keyword)> {
        let keywords = st
            .child_ids(self.syn_id)
            .into_iter()
            .filter_map(|&child| match st.as_token(child)?.kind() {
                TokenKind::Keyword(keyword) => Some(keyword),
                _ => None,
            })
            .take(3)
            .collect::<Vec<_>>();
        if keywords.len() != 2 {
            return None;
        }
        Some((keywords[0], keywords[1]))
    }
}

impl Val {
    pub(crate) fn int(&self, st: &SynTree<'_>) -> Option<i64> {
        st.child_ids(self.syn_id)
            .into_iter()
            .filter(|&&child| {
                st.as_token(child)
                    .into_iter()
                    .any(|token| token.kind() == TokenKind::Int)
            })
            .map(|&child| st.node_text(child).parse::<i64>().unwrap())
            .next()
    }
}

impl ModuleDecl {
    pub(crate) fn func_decls(&self, st: &SynTree<'_>) -> Vec<FuncDecl> {
        st.cast_children(self.syn_id)
    }

    pub(crate) fn export_decls(&self, st: &SynTree<'_>) -> Vec<ExportDecl> {
        st.cast_children(self.syn_id)
    }
}

impl FuncDecl {
    pub(crate) fn name(&self, st: &SynTree<'_>) -> Option<Name> {
        st.cast_child(self.syn_id)
    }

    pub(crate) fn result_tys(&self, st: &SynTree<'_>) -> Vec<ResultTy> {
        st.cast_children(self.syn_id)
    }

    pub(crate) fn instrs(&self, st: &SynTree<'_>) -> Vec<Instr> {
        st.cast_children(self.syn_id)
    }
}

impl ExportDecl {
    pub(crate) fn name(&self, st: &SynTree<'_>) -> Option<String> {
        let child = st
            .child_ids(self.syn_id)
            .into_iter()
            .filter(|&&child| {
                st.as_token(child)
                    .into_iter()
                    .any(|token| token.kind() == TokenKind::Str)
            })
            .next()
            .cloned()?;
        let text = st.node_text(child);
        assert!(text.starts_with("\"") && text.ends_with("\""));
        Some(text[1..text.len() - 1].to_string())
    }

    pub(crate) fn desc(&self, st: &SynTree<'_>) -> Option<ExportDesc> {
        st.cast_child(self.syn_id)
    }
}

impl ExportDesc {
    pub(crate) fn func_keyword(&self, st: &SynTree<'_>) -> Option<Token> {
        st.find_first_token_by_kind(self.syn_id, TokenKind::Keyword(Keyword::Func))
    }

    pub(crate) fn name(&self, st: &SynTree<'_>) -> Option<Name> {
        st.cast_child(self.syn_id)
    }
}
