use super::*;

#[derive(Clone, Debug)]
pub(crate) struct ModuleDecl {
    syn_id: SynId,
}

#[derive(Clone, Debug)]
pub(crate) struct FuncDecl {
    syn_id: SynId,
}

#[derive(Clone, Debug)]
pub(crate) struct Name {
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

impl ModuleDecl {
    pub(crate) fn func_decls(&self, st: &SynTree<'_>) -> Vec<FuncDecl> {
        st.cast_children(self.syn_id)
    }
}

impl FuncDecl {
    pub(crate) fn name(&self, st: &SynTree<'_>) -> Option<Name> {
        st.cast_child(self.syn_id)
    }
}
