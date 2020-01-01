module RaiiLang.Cli.Program

open System
open System.IO
open RaiiLang.SyntaxLower
open RaiiLang.SyntaxParse
open RaiiLang.Kir
open RaiiLang.KirEval
open RaiiLang.KirGen

[<EntryPoint>]
let main _ =
  let sourceCode = File.ReadAllText("tests/inc/inc.raii")
  sourceCode |> parse |> lower |> kirGen |> kirEval |> ignore
  0
