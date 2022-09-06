module rec OptLang.MirDump

open System.Collections.Generic
open System.Text
open OptLang.Mir
open OptLang.Symbol

let private lookup key map =
  match map |> Map.tryFind key with
  | Some it -> it
  | None -> failwithf "unreachable. Missing key: %A" key

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private Dx =
  { Output: StringBuilder
    mutable Depth: int
    mutable Newline: bool }

let private newDx () : Dx =
  { Output = StringBuilder()
    Depth = 0
    Newline = false }

let private newline (dx: Dx) =
  if dx.Newline then
    dx.Output.Append('\n') |> ignore

  dx.Newline <- true

let private write (dx: Dx) (s: string) =
  if dx.Newline then
    dx.Output.Append('\n') |> ignore

    for _ in 1 .. dx.Depth * 2 do
      dx.Output.Append(' ') |> ignore

    dx.Newline <- false

  dx.Output.Append(s) |> ignore

let private writeLn (dx: Dx) (s: string) =
  if s <> "" then write dx s
  newline dx

let private deep (dx: Dx) = dx.Depth <- dx.Depth + 1
let private shallow (dx: Dx) = dx.Depth <- dx.Depth - 1

let private displayTy (mir: MProgram) ty : string =
  let rec go ty =
    match ty with
    | MTy.Void -> "void"
    | MTy.Bool -> "bool"
    | MTy.Int -> "int"
    | MTy.String -> "string"

    | MTy.Record record -> (mir.Records |> lookup record).Name

    | MTy.Array array ->
      let itemTy = go mir.Arrays.[array.Index].ItemTy
      $"array({itemTy})"

  go ty

let displayPlace (mir: MProgram) (place: MPlace) : string =
  let sb = StringBuilder()

  sb.Append(string place.Local) |> ignore

  for part in place.Path do
    match part with
    | MPart.Index (index, _) ->
      let index = displayRval mir index
      sb.Append($"[{index}]") |> ignore

    | MPart.Field (index, record) ->
      let field = (mir.Records |> lookup record).Fields.[index]
      sb.Append($".{field}") |> ignore

  sb.ToString()

let displayRval (mir: MProgram) (rval: MRval) : string =
  match rval with
  | MRval.Void -> "void"
  | MRval.Bool value -> string value
  | MRval.Int value -> string value
  | MRval.String value -> sprintf "%A" value
  | MRval.Read place -> "read " + displayPlace mir place

  | MRval.Unary (unary, rval) ->
    match (match unary with
           | MUnary.Minus -> Ok "-"
           | MUnary.Not -> Ok "!"

           | MUnary.ArrayLen ->
             let rval = displayRval mir rval
             Error $"({rval}).length")
      with
    | Ok op -> op + displayRval mir rval
    | Error it -> it

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

    let lhs = displayRval mir lhs
    let rhs = displayRval mir rhs
    $"({lhs} {b} {rhs})"

  | MRval.Record (fields, record) ->
    let fields =
      (mir.Records |> lookup record).Fields
      |> Array.zip fields
      |> Array.map (fun (value, field) -> field.Name + " = " + displayRval mir value)
      |> String.concat "; "

    record.Name + "{" + fields + "}"

  | MRval.Array (items, array) ->
    let itemTy = mir.Arrays.[array.Index].ItemTy |> string

    let items =
      items
      |> Array.map (displayRval mir)
      |> String.concat ", "

    $"array({itemTy})[{items}]"

let displayStmt (mir: MProgram) stmt =
  match stmt with
  | MStmt.Assign (dest, value) ->
    let dest = displayPlace mir dest
    let value = displayRval mir value
    $"set {dest} <- {value}"

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
       |> Array.map (displayRval mir)
       |> String.concat ", ")

  | MStmt.Call (place, callable, args) ->
    sprintf
      "%s <- call %A(%s)"
      (displayPlace mir place)
      callable
      (args
       |> Array.map (displayRval mir)
       |> String.concat ", ")

let displayTerminator mir terminator =
  match terminator with
  | MTerminator.Unreachable -> "unreachable"
  | MTerminator.Goto label -> $"goto {label}"
  | MTerminator.Return -> "return"

  | MTerminator.If (cond, body, alt) ->
    let cond = displayRval mir cond
    $"goto if {cond} then {body} else {alt}"

let private onFnDef (mir: MProgram) (dx: Dx) (fnDef: MFnDef) =
  writeLn dx "locals:"

  do
    deep dx

    for KeyValue (local, localDef) in fnDef.Locals do
      let name = local.Name
      let ty = displayTy mir localDef.Ty
      writeLn dx $"{name}: {ty}"

    shallow dx
    newline dx

  do
    writeLn dx "blocks:"
    deep dx

    for i, blockDef in Array.indexed fnDef.Blocks do
      writeLn dx $"B{i}:"
      deep dx

      for stmt in blockDef.Stmts do
        writeLn dx (displayStmt mir stmt)

      writeLn dx (displayTerminator mir blockDef.Terminator)
      shallow dx

    shallow dx

let dumpMir (mir: MProgram) : string =
  let dx: Dx = newDx ()

  for fn, fnDef in Map.toList mir.Fns do
    if fnDef.TopLevel then
      writeLn dx $"body"
    else
      let paramList =
        fnDef.Params
        |> Array.map (fun (param, ty) -> param.Name + ": " + displayTy mir ty)
        |> String.concat ", "

      let resultTy = displayTy mir fnDef.ResultTy

      writeLn dx $"fn {fn.Name}({paramList}) -> {resultTy}"

    do
      deep dx
      onFnDef mir dx fnDef
      shallow dx

    newline dx

  newline dx
  dx.Output.ToString()
