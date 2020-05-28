use super::*;
use crate::binary;

struct Assemble<'a> {
    st: &'a SynTree<'a>,
}

impl<'a> Assemble<'a> {
    fn get_module_decl(&self) -> ast::ModuleDecl {
        self.st
            .cast_child::<ast::ModuleDecl>(self.st.root_id())
            .unwrap()
    }

    fn get_func_decls(&self) -> Vec<ast::FuncDecl> {
        self.get_module_decl().func_decls(self.st)
    }

    fn get_export_decls(&self) -> Vec<ast::ExportDecl> {
        self.get_module_decl().export_decls(self.st)
    }

    fn ty_section(&self) -> binary::TySection {
        let mut func_tys = vec![];
        for func_decl in self.get_func_decls() {
            let result_tys = func_decl
                .result_tys(self.st)
                .into_iter()
                .map(|result_ty| {
                    debug_assert!(result_ty.is_i32(self.st));
                    binary::ValTy::I32
                })
                .collect();
            func_tys.push(binary::FuncTy {
                param_tys: vec![],
                result_tys,
            });
        }
        binary::TySection { func_tys }
    }

    fn func_section(&self) -> binary::FuncSection {
        let func_ty_indexes = self
            .get_func_decls()
            .into_iter()
            .enumerate()
            .map(|(i, _)| i)
            .collect();
        binary::FuncSection { func_ty_indexes }
    }

    fn export_section(&self) -> binary::ExportSection {
        let exports = self
            .get_export_decls()
            .into_iter()
            .map(|export_decl| {
                let name = export_decl.name(self.st).expect("noname in export decl");
                let desc = export_decl.desc(self.st).unwrap();
                assert!(desc.func_keyword(self.st).is_some());
                (name, binary::ExportDesc::Func(0))
            })
            .collect();
        binary::ExportSection { exports }
    }

    fn code_section(&self) -> binary::CodeSection {
        let codes = self
            .get_func_decls()
            .into_iter()
            .map(|func_decl| {
                let locals = vec![];
                let body = func_decl
                    .instrs(self.st)
                    .into_iter()
                    .map(
                        |instr| match instr.op(self.st).unwrap().pair(self.st).unwrap() {
                            (Keyword::I32, Keyword::Const) => {
                                let value = instr.val(self.st).unwrap().int(self.st).unwrap();
                                binary::Instr::I32Const(value as i32)
                            }
                            (Keyword::I32, Keyword::Add) => binary::Instr::I32Add,
                            pair => panic!("unknown instr {:?}", pair),
                        },
                    )
                    .collect();
                binary::Code { locals, body }
            })
            .collect();
        binary::CodeSection { codes }
    }

    fn root(&self) -> binary::Root {
        binary::Root {
            ty_section: Some(self.ty_section()),
            func_section: Some(self.func_section()),
            export_section: Some(self.export_section()),
            code_section: Some(self.code_section()),
        }
    }
}

pub(crate) fn assemble(st: &SynTree<'_>) -> binary::Root {
    Assemble { st }.root()
}
