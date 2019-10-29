pub(crate) mod cir {
    use std::io::{self, Write};

    #[derive(Clone, Debug)]
    pub(crate) enum CTy {
        Void,
        Bool,
        Int,
        Enum {
            ident: String,
            variants: Option<Vec<(String, CTy)>>,
        },
    }

    #[derive(Clone, Debug)]
    pub(crate) enum CExpr {
        Bool(bool),
        Int(i64),
        Ident(String),
        Call {
            cal: Box<CExpr>,
            args: Vec<CExpr>,
        },
        Neg(Box<CExpr>),
        ExprStmt(CTy),
        BlockStmt(Vec<CExpr>),
        RetStmt(Option<Box<CExpr>>),
        IfStmt {
            cond: Box<CExpr>,
            body: Box<CExpr>,
        },
        FnDecl {
            ident: String,
            params: Vec<(String, CTy)>,
            result_ty: CTy,
            body: Box<CExpr>,
        },
        SemiDecl(Vec<CExpr>),
    }

    fn write_indent(out: &mut Vec<u8>, indent: usize) -> io::Result<()> {
        for _ in 0..indent * 4 {
            write!(out, " ")?;
        }
        Ok(())
    }

    impl CTy {
        pub fn write(&self, out: &mut Vec<u8>, indent: usize) -> io::Result<()> {
            match self {
                CTy::Void => write!(out, "void"),
                CTy::Bool => write!(out, "bool"),
                CTy::Int => write!(out, "int"),
                CTy::Enum { ident, variants } => {
                    write!(out, "enum {}", ident)?;
                    if let Some(variants) = variants {
                        write!(out, " {{\n")?;
                        for (ident, ty) in variants {
                            let indent = indent + 1;
                            write_indent(out, indent)?;
                            write!(out, "{} ", ident)?;
                            ty.write(out, indent)?;
                            write!(out, ",\n")?;
                        }
                        write_indent(out, indent)?;
                        write!(out, "}}")?;
                    }
                    Ok(())
                }
            }
        }
    }

    impl CExpr {
        pub fn write(&self, out: &mut Vec<u8>, indent: usize) -> io::Result<()> {
            match self {
                CExpr::Bool(false) => write!(out, "false"),
                CExpr::Bool(true) => write!(out, "true"),
                CExpr::Int(value) => write!(out, "{}", value),
                CExpr::Ident(ident) => write!(out, "{}", ident),
                CExpr::Call { cal, args } => {
                    cal.write(out, indent)?;
                    write!(out, "(")?;

                    let mut first = true;
                    for arg in args {
                        if !first {
                            write!(out, ", ")?;
                        }
                        first = false;

                        arg.write(out, indent)?;
                    }
                    write!(out, ")")
                }
                CExpr::Neg(arg) => {
                    write!(out, "!")?;
                    arg.write(out, indent)
                }
                CExpr::ExprStmt(body) => {
                    body.write(out, indent)?;
                    write!(out, ";")
                }
                CExpr::BlockStmt(body) => {
                    if body.is_empty() {
                        write!(out, "{{}}")
                    } else {
                        write!(out, "{{\n")?;
                        for expr in body.iter() {
                            let indent = indent + 1;
                            write_indent(out, indent)?;
                            expr.write(out, indent);
                            write!(out, ";\n")?;
                        }
                        write_indent(out, indent)?;
                        write!(out, "}}")
                    }
                }
                CExpr::RetStmt(None) => write!(out, "return;"),
                CExpr::RetStmt(Some(arg)) => {
                    write!(out, "return ")?;
                    arg.write(out, indent)?;
                    write!(out, ";")
                }
                CExpr::IfStmt { cond, body } => {
                    write!(out, "if (")?;
                    cond.write(out, indent)?;
                    write!(out, ") ")?;
                    body.write(out, indent)
                }
                CExpr::FnDecl {
                    ident,
                    params,
                    result_ty,
                    body,
                } => {
                    result_ty.write(out, indent)?;
                    write!(out, " {}(", ident)?;

                    let mut first = true;
                    for (param, ty) in params {
                        if !first {
                            write!(out, ", ")?;
                        }
                        first = false;

                        ty.write(out, indent)?;
                        write!(out, " {}", param)?;
                    }

                    write!(out, ") ")?;
                    body.write(out, indent)
                }
                CExpr::SemiDecl(decls) => {
                    let mut first = false;
                    for decl in decls {
                        if !first {
                            write!(out, "\n")?;
                        }
                        first = false;

                        write_indent(out, indent)?;
                        decl.write(out, indent)?;
                        write!(out, "\n")?;
                    }
                    Ok(())
                }
            }
        }
    }
}

pub(crate) mod functions {
    use crate::syntax::*;

    fn collect_atoms(node: &Node) -> Vec<String> {
        fn go(node: &Node, out: &mut Vec<String>) {
            // match node {
            //     Node::Token(token) => {
            //         if token.kind() == TokenKind::Atom {
            //             out.push(token.text().to_string());
            //         }
            //     }
            //     Node::Expr { children, .. } => {
            //         for child in children {
            //             go(child, out);
            //         }
            //     }
            // }
        }

        let mut atoms = vec![];
        go(node, &mut atoms);
        atoms.sort();
        atoms.dedup();
        atoms
    }



    pub(crate) fn compile(node: &Node) -> String {
        "".to_string()
    }
}
