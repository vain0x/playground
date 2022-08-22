module rec OptLang.Program

open System.IO
open System.Text
open OptLang.MirGen
open OptLang.Parse
open OptLang.Symbol
open OptLang.Tokenize
open OptLang.TypeCheck
open OptLang.Inline

module T = OptLang.Tir
module M = OptLang.Mir

let private lookup key map =
  match map |> Map.tryFind key with
  | Some it -> it
  | None -> failwithf "unreachable. Missing key: %A" key

type private Node =
  | Text of string
  | Label of string * Node
  | List of Node list
  override this.ToString() = renderNode this

let private renderNode node =
  let rec go (sb: StringBuilder) (indent: string) node =
    match node with
    | Text text -> sb.Append(text) |> ignore

    | Label (label, content) ->
      sb.Append(label).Append(":") |> ignore
      go sb indent content

    | List [] -> sb.Append("[]") |> ignore

    | List (head :: tail) ->
      let deep = indent + "  "

      sb.Append(deep).Append("- ") |> ignore
      go sb deep head

      for item in tail do
        sb.Append(deep).Append("- ") |> ignore
        go sb deep item

  let sb = StringBuilder()
  go sb "\n" node
  sb.ToString()

let private dumpTy ty =
  let rec go ty =
    match ty with
    | T.Ty.Void -> "void"
    | T.Ty.Bool -> "bool"
    | T.Ty.Int -> "int"
    | T.Ty.String -> "string"
    | T.Ty.Record record -> string record
    | T.Ty.Array item -> "array(" + go item + ")"

  go ty

let private dumpTir tir =
  let onLocals locals =
    locals
    |> List.map (fun (name, ty) -> Text(string (name: Symbol) + ": " + dumpTy ty))

  let rec go decl =
    match decl with
    | T.Decl.Block (locals, _) -> Label("block", List(onLocals locals))
    | T.Decl.Fn (name, _, _, locals, _) -> Label("fn " + string name, List(onLocals locals))
    | T.Decl.RecordTy _ -> List []

  tir |> List.map go |> List

let private dumpMTy (mir: M.MProgram) ty =
  let rec go ty =
    match ty with
    | M.MTy.Void -> "void"
    | M.MTy.Bool -> "bool"
    | M.MTy.Int -> "int"
    | M.MTy.String -> "string"
    | M.MTy.Record record -> (mir.Records |> lookup record).Name
    | M.MTy.Array array ->
      "array("
      + go mir.Arrays.[array.Index].ItemTy
      + ")"

  go ty

let private dumpMir (mir: M.MProgram) =
  let onLocals locals =
    locals
    |> List.map (fun (_, local: M.LocalDef) -> Label(local.Name, Text(dumpMTy mir local.Ty)))

  let rec displayPlace (place: M.MPlace) =
    let sb = StringBuilder()

    sb.Append(string place.Local) |> ignore

    for part in place.Path do
      match part with
      | M.Part.Index (index, _) ->
        sb
          .Append("[")
          .Append((displayRval index: string))
          .Append("]")
        |> ignore
      | M.Part.Field (index, record) ->
        sb
          .Append(".")
          .Append(
            (mir.Records |> lookup record).Fields.[index]
            |> string
          )
        |> ignore

    sb.ToString()

  and displayRval (rval: M.MRval) =
    match rval with
    | M.MRval.Void -> "void"
    | M.MRval.Bool value -> string value
    | M.MRval.Int value -> string value
    | M.MRval.String value -> sprintf "%A" value
    | M.MRval.Read place -> "read " + displayPlace place

    | M.MRval.Unary (unary, rval) ->
      let unary =
        match unary with
        | M.MUnary.Minus -> "-"
        | M.MUnary.Not -> "!"
        | M.MUnary.ArrayLen -> "(.len)"

      unary + displayRval rval

    | M.MRval.Binary (binary, lhs, rhs) ->
      let b =
        match binary with
        | M.MBinary.Add -> "+"
        | M.MBinary.Subtract -> "-"
        | M.MBinary.Multiply -> "*"
        | M.MBinary.Divide -> "/"
        | M.MBinary.Modulo -> "%"
        | M.MBinary.Equal -> "=="
        | M.MBinary.NotEqual -> "!="
        | M.MBinary.LessThan -> "<"
        | M.MBinary.LessEqual -> "<="
        | M.MBinary.GreaterThan -> ">"
        | M.MBinary.GreaterEqual -> ">="

      "("
      + displayRval lhs
      + " "
      + b
      + " "
      + displayRval rhs
      + ")"

    | M.MRval.Record (fields, record) ->
      (mir.Records |> lookup record).Fields
      |> Array.zip fields
      |> Array.map (fun (value, field) -> field.Name + " = " + displayRval value)
      |> String.concat "; "
      |> (fun x -> record.Name + "{" + x + "}")

    | M.MRval.Array (items, array) ->
      let itemTy = mir.Arrays.[array.Index].ItemTy |> string

      let items =
        items
        |> Array.map displayRval
        |> String.concat ", "

      "array(" + itemTy + ")[" + items + "]"

  let onStmtList stmtList =
    stmtList
    |> List.map (fun stmt ->
      match stmt with
      | M.MStmt.Assign (dest, value) -> sprintf "set %s <- %s" (displayPlace dest) (displayRval value)

      | M.MStmt.Call (callable, args) ->
        sprintf
          "call %A(%s)"
          callable
          (args
           |> Array.map displayRval
           |> String.concat ", "))
    |> List.map Text

  let onTerminator terminator =
    match terminator with
    | M.MTerminator.Unreachable -> "unreachable"
    | M.MTerminator.Goto label -> "goto " + string label
    | M.MTerminator.Return -> "return"

    | M.MTerminator.If (cond, body, alt) ->
      "goto if "
      + displayRval cond
      + " then "
      + string body
      + " else "
      + string alt

  let onBlocks blocks =
    blocks
    |> List.mapi (fun i (block: M.BlockDef) ->
      Label(
        $"B{i}",
        List(
          List.append
            (onStmtList (Array.toList block.Stmts))
            [ Label("terminator", Text(onTerminator block.Terminator)) ]
        )
      ))

  let onBody locals blocks =
    List [ Label("locals", List(onLocals locals))
           Label("blocks", List(onBlocks blocks)) ]

  List.append
    (mir.Bodies
     |> Array.map (fun (body: M.BodyDef) -> Label("body", onBody (Map.toList body.Locals) (List.ofArray body.Blocks)))
     |> Array.toList)
    (mir.Fns
     |> Map.toList
     |> List.map (fun (_, fn: M.FnDef) -> Label("fn " + fn.Name, onBody (Map.toList fn.Locals) (List.ofArray fn.Blocks))))
  |> List

[<EntryPoint>]
let main _ =
  for file in Directory.GetFiles("tests") do
    if Path.GetExtension(file) = ".opt" then
      eprintfn "file: %s" file
      let contents = File.ReadAllText(file)
      let tokens = tokenize contents |> List.toArray
      let ast = parseTokens tokens
      let tir = typeCheck ast
      let mir = genMir tir
      let mir = performInlineExpansion mir

      let output =
        sprintf "ast: %A\n\n\n\ntir: %A\n\n\n\nmir: %A\n" ast (dumpTir tir) (dumpMir mir)

      File.WriteAllText(Path.ChangeExtension(file, "txt"), output)

  0
