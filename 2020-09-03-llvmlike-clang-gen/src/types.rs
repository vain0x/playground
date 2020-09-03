pub(crate) struct FloatType<'ctx> {
    name: &'ctx str,
}

pub(crate) enum BasicTypeEnum<'ctx> {
    // ArrayType(ArrayType<'ctx>),
    FloatType(FloatType<'ctx>),
    // IntType(IntType<'ctx>),
    // PointerType(PointerType<'ctx>),
    // StructType(StructType<'ctx>),
    // VectorType(VectorType<'ctx>),
}
