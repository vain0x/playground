module Inspector

open System
open AST
open Parser

    type AST.Expr with
        member this.Inspect() =
            let inspect (x: AST.Expr) = x.Inspect()
            match this with
            | Nothing -> "()"
            | IntLit n -> string n
            | Ident s  -> "``" + s + "``"
            | IdentPtn s -> "\\``" + s + "``"
            | Tuple es ->
                "(" + String.Join(",", List.map inspect es) + ")"

            ///中置演算子
            | AppPr (AppPr(Ident op_name, lhs), rhs)
                when Ops.is_infix_op op_name
                ->
                "(" + (inspect lhs) + " " + op_name + " " + (inspect rhs) + ")"

            | AppPr (lhs, rhs) ->
                "(" + (inspect lhs) + " " + (inspect rhs) + ")"

            | AppPo
            | If _ -> failwith "unsupported"
