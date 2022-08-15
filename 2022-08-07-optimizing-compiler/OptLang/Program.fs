module rec OptLang.Program

open System.IO
open System.Text
open OptLang.Parse
open OptLang.Symbol
open OptLang.Tokenize
open OptLang.TypeCheck

module T = OptLang.Tir

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

[<EntryPoint>]
let main _ =
  for file in Directory.GetFiles("tests") do
    if Path.GetExtension(file) = ".opt" then
      eprintfn "file: %s" file
      let contents = File.ReadAllText(file)
      let tokens = tokenize contents |> List.toArray
      let ast = parseTokens tokens
      let tir = typeCheck ast

      let output = sprintf "ast: %A\n\n\n\ntir: %A\n" ast (dumpTir tir)
      File.WriteAllText(Path.ChangeExtension(file, "txt"), output)

  0
