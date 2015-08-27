module StEval

    open AST
    open Value
    open Basis.Core
    open Util

    let try_lookup env name =
        env |> Map.tryFind name

    let rec value_from_expr env =
        function
        | IntLit n -> Value.Int n
        | Ident name ->
            try_lookup env name
            |> Option.getOrElse (fun() -> failwith <| "Undefined identifier: " + name)
        | IdentPtnLit name ->
            Value.IdentPtn name
        | ListLit ls ->
            ls |> List.map (value_from_expr env) |> Value.List
        | DictLit dt ->
            dt |> Map.map (konst <| value_from_expr env) |> Value.Dict 
        | AppPr (f, x) ->
            Thunk (x, lazy value_from_expr env x)
        | AppPo
        | If _ -> failwith "Unsupported"

    let st_eval env =
        ()
