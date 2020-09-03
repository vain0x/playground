// https://thedan64.github.io/inkwell/inkwell/index.html

use context::Context;

mod builder;
mod context;
mod module;
mod passes;
mod types;
mod values;

mod others {
    pub(crate) struct FloatPredicate;
    pub(crate) enum OptimizationLevel {}
}

mod ast {
    pub(crate) struct AParam {
        pub(crate) name: String,
        // pub(crate) ty: ATy,
    }

    pub(crate) enum AExpr {
        Int(i32),
        Var(String),
        Add(Box<AExpr>, Box<AExpr>),
    }

    pub(crate) struct AFnDecl {
        pub(crate) params: Vec<AParam>,
        pub(crate) body: Option<AExpr>,
    }
}

mod my_compiler {
    use super::ast::*;
    use super::*;
    use crate::builder::Builder;
    use crate::context::Context;
    use crate::module::Module;
    use crate::types::*;
    use crate::values::*;
    use std::collections::HashMap;

    pub(crate) struct Compiler<'a, 'ctx> {
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        // fpm: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
        function: &'a AFnDecl,
        variables: HashMap<String, PointerValue<'ctx>>,
        fn_value_opt: Option<FunctionValue<'ctx>>,
    }

    pub(crate) fn compiler_new<'a, 'ctx>(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        // fpm: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
        function: &'a AFnDecl,
    ) -> Compiler<'a, 'ctx> {
        Compiler {
            context,
            builder,
            module,
            function,
            variables: Default::default(),
            fn_value_opt: Default::default(),
        }
    }

    pub(crate) fn compile_fn<'ctx>(
        compiler: &'ctx Compiler,
    ) -> Result<FunctionValue<'ctx>, &'static str> {
        todo!()
    }
}

fn main() {
    use crate::ast::*;

    let context = Context::create();
    let module = context.create_module("repl");
    let builder = context.create_builder();

    let function = ast::AFnDecl {
        params: vec![AParam { name: "x".into() }],
        body: Some(AExpr::Int(1)),
    };

    let mut compiler = my_compiler::compiler_new(&context, &builder, &module, &function);
    my_compiler::compile_fn(&mut compiler).unwrap();
}
