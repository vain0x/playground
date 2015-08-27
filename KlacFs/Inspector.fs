module Inspector

open System
open AST
open Parser
open Basis.Core

    type AST.Expr with
        member this.Inspect() =
            let inspect (x: AST.Expr) = x.Inspect()
            match this with
            | IntLit n -> string n
            | Ident s  -> "``" + s + "``"
            | IdentPtn s -> "\\``" + s + "``"
            | List es ->
                "(" + (es |> List.map inspect |> Str.join ", ")
                + (if es |> List.length = 1 then ",)" else ")")
            | Dict dt ->
                let kvs =
                    dt  |> Map.toList
                        |> List.map (fun (k, v) -> k + ": " + (inspect v))
                "{" + (kvs |> Str.join ", ") + "}"

            ///中置演算子
            | AppPr (AppPr(Ident op_name, lhs), rhs)
                when Ops.is_infix_op op_name
                ->
                "(" + (inspect lhs) + " " + op_name + " " + (inspect rhs) + ")"

            | AppPr (lhs, rhs) ->
                "(" + (inspect lhs) + " " + (inspect rhs) + ")"

            | AppPo
            | If _ -> failwith "unsupported"
