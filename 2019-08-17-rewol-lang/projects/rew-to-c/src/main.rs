use rew::Expr;
use std::io::{self, BufWriter, Write};

#[derive(Clone, Debug)]
enum CExpr {
    IntExpr(i64),
    ReturnStmt { result: Option<Box<CExpr>> },
    FnDecl { name: String, body: Vec<CExpr> },
}

fn compile(expr: Expr) -> CExpr {
    match expr {
        Expr::IntExpr(value) => CExpr::IntExpr(value),
        Expr::ReturnStmt { result } => CExpr::ReturnStmt {
            result: Some(Box::new(compile(*result))),
        },
        Expr::FnDecl { name, body } => {
            let name = if name == "__main" {
                "main".to_string()
            } else {
                name
            };
            CExpr::FnDecl {
                name,
                body: body.into_iter().map(compile).collect(),
            }
        }
    }
}

fn emit(expr: &CExpr, indent: &str, out: &mut impl Write) -> io::Result<()> {
    match expr {
        CExpr::IntExpr(value) => write!(out, "{}", value),
        CExpr::ReturnStmt { result: None } => unimplemented!("return;"),
        CExpr::ReturnStmt {
            result: Some(result),
        } => {
            write!(out, "{}return ", indent)?;
            emit(result, indent, out)?;
            write!(out, ";\n")
        }
        CExpr::FnDecl { name, body } => {
            write!(out, "int {}(void) {{\n", name)?;
            for stmt in body {
                emit(stmt, "    ", out)?;
            }
            write!(out, "}}\n")
        }
    }
}

fn main() {
    let expr = compile(rew::compile());
    let mut out = vec![];
    emit(&expr, "", &mut out).unwrap();

    let stdout = io::stdout();
    let mut stdout = BufWriter::new(stdout);
    stdout.write_all(&out).unwrap();
}
