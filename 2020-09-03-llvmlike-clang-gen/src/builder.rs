use crate::context::Context;
use std::marker::PhantomData;

pub(crate) struct Builder<'ctx> {
    phantom: PhantomData<&'ctx ()>,
}

impl<'ctx> Builder<'ctx> {
    pub(crate) fn new(context: &'ctx Context) -> Self {
        Self {
            phantom: PhantomData,
        }
    }

    pub(crate) fn build_return(&mut self) {
        //
    }
}
