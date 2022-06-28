module rec Pcc.Program

open System.IO
open Pcc.Parser

let inline private unreachable context = failwithf "unreachable: %A" context

// -----------------------------------------------
// AST
// -----------------------------------------------

[<EntryPoint>]
let main _ =
  let args = System.Environment.GetCommandLineArgs()
  let lexText = File.ReadAllText(args.[1])
  // let grammarText = File.ReadAllText(args.[2])

  let lexer = MyLex.NfaLexer.parse lexText
  // let parser = MyYacc.generateParser grammarText

  let parseString input =
    let tokens = MyLex.NfaLexer.tokenize input lexer
    // let root = MyYacc.parseTokensToTree (Array.ofList tokens) parser
    // AstLower.lowerRoot root
    parseTokens (Array.ofList tokens)

  for name in
    [ "single_assignment"
      "fizz_buzz"
      "local_functions"
      "multiple_statements" ] do
    let pathname = $"tests/syntax/{name}.simple"
    let input = File.ReadAllText(pathname)
    let outputPathname = $"tests/syntax/{name}_ast.txt"

    eprintfn "------------------------"
    eprintfn "file: %s" pathname

    let ast = parseString input
    File.WriteAllText(outputPathname, string ast)

  0
