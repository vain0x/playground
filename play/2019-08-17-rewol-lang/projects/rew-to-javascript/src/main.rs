use rew::Expr;
use std::io::{self, BufWriter, Write};

#[derive(Clone, Debug)]
enum JsExpr {
    IntExpr(i64),
    ReturnStmt { result: Option<Box<JsExpr>> },
    FnDecl { name: String, body: Vec<JsExpr> },
}

fn compile(expr: Expr) -> JsExpr {
    match expr {
        Expr::IntExpr(value) => JsExpr::IntExpr(value),
        Expr::ReturnStmt { result } => JsExpr::ReturnStmt {
            result: Some(Box::new(compile(*result))),
        },
        Expr::FnDecl { name, body } => JsExpr::FnDecl {
            name,
            body: body.into_iter().map(compile).collect(),
        },
    }
}

fn emit(expr: &JsExpr, indent: &str, out: &mut impl Write) -> io::Result<()> {
    match expr {
        JsExpr::IntExpr(value) => write!(out, "{}", value),
        JsExpr::ReturnStmt { result: None } => unimplemented!("return;"),
        JsExpr::ReturnStmt {
            result: Some(result),
        } => {
            write!(out, "{}return ", indent)?;
            emit(result, indent, out)?;
            write!(out, "\n")
        }
        JsExpr::FnDecl { name, body } => {
            write!(out, "const {} = () => {{\n", name)?;
            for stmt in body {
                emit(stmt, "    ", out)?;
            }
            write!(out, "}}\n\n")
        }
    }
}

fn main() {
    let expr = compile(rew::compile());
    let mut out = vec![];
    emit(&expr, "", &mut out).unwrap();
    write!(out, "__main()\n").unwrap();

    let stdout = io::stdout();
    let mut stdout = BufWriter::new(stdout);
    stdout.write_all(&out).unwrap();
}
