use crate::{context::ContextRef, values::*};
use std::marker::PhantomData;

pub(crate) struct Module<'ctx> {
    name: String,
    functions: Vec<FunctionValue<'ctx>>,
    context: ContextRef<'ctx>,
    phantom: PhantomData<&'ctx ()>,
}

impl<'ctx> Module<'ctx> {
    pub(crate) fn new(context: ContextRef<'ctx>, name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            functions: Default::default(),
            context,
            phantom: PhantomData,
        }
    }

    pub(crate) fn get_context(&self) -> ContextRef<'ctx> {
        todo!()
    }

    pub(crate) fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        todo!()
    }

    pub(crate) fn add_function(&mut self, name: impl Into<String>) {
        self.functions.push(todo!())
    }
}
