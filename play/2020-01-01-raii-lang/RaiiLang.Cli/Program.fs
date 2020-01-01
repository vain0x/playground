module RaiiLang.Cli.Program

open System
open System.IO
open RaiiLang.SyntaxParse

[<EntryPoint>]
let main _ =
  let sourceCode = File.ReadAllText("tests/inc/inc.raii")
  sourceCode |> parse |> printfn "%A"
  0
