module rec OptLang.Program

open System.IO
open System.Text
open OptLang.Inline
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
        sprintf "ast: %A\n\n\n\ntir: %A\n\n\n\nmir: %s\n" ast (dumpTir tir) (MirDump.dumpMir mir)

      File.WriteAllText(Path.ChangeExtension(file, "txt"), output)

      Interpret.interpret mir

  0
