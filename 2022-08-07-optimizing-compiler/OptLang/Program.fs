module rec OptLang.Program

open System.IO
open OptLang.Parse
open OptLang.Syntax
open OptLang.Tokenize

[<EntryPoint>]
let main _ =
  for file in Directory.GetFiles("tests") do
    if Path.GetExtension(file) = ".opt" then
      eprintfn "file: %s" file
      let contents = File.ReadAllText(file)
      let tokens = tokenize contents |> List.toArray
      let ast = parseTokens tokens

      File.WriteAllText(Path.ChangeExtension(file, "txt"), sprintf "%A\n" ast)

  0
