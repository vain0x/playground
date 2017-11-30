module AST

    type Expr =
        ///no value
        | Nothing

        ///intger literal
        //TODO: 多倍長整数にする
        | IntLit of int

        ///identifier: x
        | Ident of string

        ///identifier pattern: \x
        | IdentPtn of string

        ///tuple literal
        ///It has 2 or more values.
        | Tuple of Expr list

        ///prefix application: f x
        | AppPr of Expr * Expr

        ///postfix application: f.[...]
        ///未実装
        | AppPo //of Expr * Expr

        ///if-else 式
        ///未実装
        | If of Expr * Expr * Expr

    with
        ///infix application: x * y
        static member AppIn (op, lhs, rhs) =
            AppPr (AppPr (op, lhs), rhs)
