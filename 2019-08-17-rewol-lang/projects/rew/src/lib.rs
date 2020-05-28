#[derive(Clone)]
pub enum Expr {
    IntExpr(i64),
    ReturnStmt { result: Box<Expr> },
    FnDecl { name: String, body: Vec<Expr> },
}

pub fn compile() -> Expr {
    Expr::FnDecl {
        name: "__main".to_string(),
        body: vec![Expr::ReturnStmt {
            result: Box::new(Expr::IntExpr(0)),
        }],
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
