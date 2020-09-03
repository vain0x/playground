use std::marker::PhantomData;

pub(crate) struct PassManager<T> {
    phantom: PhantomData<T>,
}
