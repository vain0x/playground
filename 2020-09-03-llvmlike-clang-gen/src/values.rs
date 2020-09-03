use std::marker::PhantomData;

pub(crate) struct BasicValue;

pub(crate) struct BasicValueEnum;

pub(crate) struct FloatValue<'ctx> {
    phantom: PhantomData<&'ctx ()>,
}

pub(crate) struct FunctionValue<'ctx> {
    name: &'ctx str,
    phantom: PhantomData<&'ctx ()>,
}

impl<'ctx> FunctionValue<'ctx> {
    pub(crate) fn get_first_basic_block(&self) -> Option<()> {
        todo!()
    }

    pub(crate) fn get_name(&self) -> &str {
        self.name
    }

    pub(crate) fn get_param_iter(&self) -> ! {
        todo!()
    }
}

pub(crate) struct PointerValue<'ctx> {
    phantom: PhantomData<&'ctx ()>,
}
