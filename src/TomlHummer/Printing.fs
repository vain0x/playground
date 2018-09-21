module rec TomlHummer.Printing
  open System
  open TomlHummer

  let ( *- ) xs x = x :: xs

  [<RequireQualifiedAccess>]
  type ExprSyn =
    | Int
      of int
    | Ident
      of string
    | String
      of string
    | Array
      of ExprSyn list
    | Table
      of (ExprSyn list * ExprSyn) list

  [<RequireQualifiedAccess>]
  type StmtSyn =
    | Binding
      of ExprSyn list * ExprSyn
    | Table
      of ExprSyn list
    | Array
      of ExprSyn list

  let printExpr (expr: ExprSyn) (acc: string list): string list =
    match expr with
    | ExprSyn.Int value ->
      acc *- string value
    | ExprSyn.String value ->
      acc *- "\"" *- value *- "\""
    | ExprSyn.Ident value ->
      acc *- value
    | ExprSyn.Array items ->
      let rec go items acc =
        match items with
        | [] -> acc
        | [item] ->
          acc |> printExpr item
        | item :: items ->
          (acc |> printExpr item) *- ", " |> go items
      (acc *- "[" |> go items) *- "]"
    | ExprSyn.Table bindings ->
      let rec go bindings acc =
        match bindings with
        | [] -> failwith "Unreachable."
        | [path, value] ->
          acc |> printBinding path value
        | (path, value) :: bindings ->
          (acc |> printBinding path value) *- ", " |> go bindings
      match bindings with
      | [] -> acc *- "{}"
      | _ -> (acc *- "{ " |> go bindings) *- " }"

  let printPath (path: ExprSyn list) (acc: string list): string list =
    let rec go path acc =
      match path with
      | [] -> failwith "Unreachable."
      | [key] ->
        acc |> printExpr key
      | key :: path ->
        (acc |> printExpr key) *- "." |> go path
    go path acc

  let printBinding (path: ExprSyn list) (value: ExprSyn) (acc: string list): string list =
    ((acc |> printPath path) *- " = ") |> printExpr value

  let printStmts (stmts: StmtSyn list) (acc: string list): string list =
    match stmts with
    | [] -> acc
    | StmtSyn.Binding (path, value) :: stmts ->
      ((acc |> printBinding path value) *- "\n") |> printStmts stmts
    | _ -> failwith "unimpl"

  let runPrinter printer syn =
    [] |> printer syn |> Seq.rev |> String.concat ""

  let print stmts =
    runPrinter printStmts stmts
