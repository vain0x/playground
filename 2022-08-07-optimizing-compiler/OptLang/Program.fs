module rec OptLang.Program

open System.IO
open System.Text
open OptLang.Inline
open OptLang.Mir
open OptLang.MirGen
open OptLang.Parse
open OptLang.Symbol
open OptLang.Tir
open OptLang.Tokenize
open OptLang.TypeCheck

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
    | TTy.Void -> "void"
    | TTy.Bool -> "bool"
    | TTy.Int -> "int"
    | TTy.String -> "string"
    | TTy.Record record -> string record
    | TTy.Array item -> "array(" + go item + ")"

  go ty

let private dumpTir tir =
  let onLocals locals =
    locals
    |> List.map (fun (name, ty) -> Text(string (name: Symbol) + ": " + dumpTy ty))

  let rec go decl =
    match decl with
    | TDecl.Block (locals, _) -> Label("block", List(onLocals locals))
    | TDecl.Fn (name, _, _, locals, _) -> Label("fn " + string name, List(onLocals locals))
    | TDecl.RecordTy _ -> List []

  tir |> List.map go |> List

let private dumpMTy (mir: MProgram) ty =
  let rec go ty =
    match ty with
    | MTy.Void -> "void"
    | MTy.Bool -> "bool"
    | MTy.Int -> "int"
    | MTy.String -> "string"
    | MTy.Record record -> (mir.Records |> lookup record).Name
    | MTy.Array array ->
      "array("
      + go mir.Arrays.[array.Index].ItemTy
      + ")"

  go ty

let private dumpMir (mir: MProgram) =
  let onLocals locals =
    locals
    |> List.map (fun (_, local: MLocalDef) -> Label(local.Name, Text(dumpMTy mir local.Ty)))

  let rec displayPlace (place: MPlace) =
    let sb = StringBuilder()

    sb.Append(string place.Local) |> ignore

    for part in place.Path do
      match part with
      | MPart.Index (index, _) ->
        sb
          .Append("[")
          .Append((displayRval index: string))
          .Append("]")
        |> ignore
      | MPart.Field (index, record) ->
        sb
          .Append(".")
          .Append(
            (mir.Records |> lookup record).Fields.[index]
            |> string
          )
        |> ignore

    sb.ToString()

  and displayRval (rval: MRval) =
    match rval with
    | MRval.Void -> "void"
    | MRval.Bool value -> string value
    | MRval.Int value -> string value
    | MRval.String value -> sprintf "%A" value
    | MRval.Read place -> "read " + displayPlace place

    | MRval.Unary (unary, rval) ->
      let unary =
        match unary with
        | MUnary.Minus -> "-"
        | MUnary.Not -> "!"
        | MUnary.ArrayLen -> "(.len)"

      unary + displayRval rval

    | MRval.Binary (binary, lhs, rhs) ->
      let b =
        match binary with
        | MBinary.Add -> "+"
        | MBinary.Subtract -> "-"
        | MBinary.Multiply -> "*"
        | MBinary.Divide -> "/"
        | MBinary.Modulo -> "%"
        | MBinary.Equal -> "=="
        | MBinary.NotEqual -> "!="
        | MBinary.LessThan -> "<"
        | MBinary.LessEqual -> "<="
        | MBinary.GreaterThan -> ">"
        | MBinary.GreaterEqual -> ">="

      "("
      + displayRval lhs
      + " "
      + b
      + " "
      + displayRval rhs
      + ")"

    | MRval.Record (fields, record) ->
      (mir.Records |> lookup record).Fields
      |> Array.zip fields
      |> Array.map (fun (value, field) -> field.Name + " = " + displayRval value)
      |> String.concat "; "
      |> (fun x -> record.Name + "{" + x + "}")

    | MRval.Array (items, array) ->
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
      | MStmt.Assign (dest, value) -> sprintf "set %s <- %s" (displayPlace dest) (displayRval value)

      | MStmt.Call (_, callable, args) when
        (match callable with
         | MCallable.Fn fn -> (mir.Fns |> lookup fn).ResultTy = MTy.Void
         | MCallable.ArrayPush
         | MCallable.Assert _ -> true)
        ->
        sprintf
          "call %A(%s)"
          callable
          (args
           |> Array.map displayRval
           |> String.concat ", ")

      | MStmt.Call (place, callable, args) ->
        sprintf
          "%s <- call %A(%s)"
          (displayPlace place)
          callable
          (args
           |> Array.map displayRval
           |> String.concat ", "))
    |> List.map Text

  let onTerminator terminator =
    match terminator with
    | MTerminator.Unreachable -> "unreachable"
    | MTerminator.Goto label -> "goto " + string label
    | MTerminator.Return -> "return"

    | MTerminator.If (cond, body, alt) ->
      "goto if "
      + displayRval cond
      + " then "
      + string body
      + " else "
      + string alt

  let onBlocks blocks =
    blocks
    |> List.mapi (fun i (block: MBlockDef) ->
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
     |> Array.map (fun (body: MBodyDef) -> Label("body", onBody (Map.toList body.Locals) (List.ofArray body.Blocks)))
     |> Array.toList)
    (mir.Fns
     |> Map.toList
     |> List.map (fun (_, fn: MFnDef) -> Label("fn " + fn.Name, onBody (Map.toList fn.Locals) (List.ofArray fn.Blocks))))
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
