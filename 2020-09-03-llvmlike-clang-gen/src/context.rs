use crate::{builder::Builder, module::Module};
use bumpalo::Bump;

#[derive(Copy, Clone)]
pub(crate) struct ContextRef<'ctx> {
    context: &'ctx Context,
}

impl<'ctx> ContextRef<'ctx> {
    pub(crate) fn new(context: &'ctx Context) -> Self {
        Self { context }
    }

    pub(crate) fn as_ref(self) -> &'ctx Context {
        self.context
    }
}

pub(crate) struct Context {
    bump: Bump,
}

impl Context {
    pub(crate) fn new() -> Self {
        Self { bump: Bump::new() }
    }

    pub(crate) fn create() -> Self {
        Self::new()
    }

    fn get_context_ref(&self) -> ContextRef {
        ContextRef::new(self)
    }

    pub(crate) fn create_module<'ctx>(&'ctx self, name: &str) -> Module<'ctx> {
        Module::new(self.get_context_ref(), name)
    }

    pub(crate) fn create_builder(&self) -> Builder<'_> {
        todo!()
    }

    // pub(crate) fn f64_type(&self) -> BasicType {}
}
