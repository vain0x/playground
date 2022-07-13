module rec Pcc.Program

open System.Diagnostics
open System.IO
open Pcc.Parser
open Pcc.CodeGen

let inline private unreachable context = failwithf "unreachable: %A" context

let private trace msg = eprintfn "%s\n" msg

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
      "multiple_statements"
      "hello_world"
      "iprint"
      "local_functions"
      "fizz_buzz" ] do
    let pathname = $"tests/syntax/{name}.simple"
    let input = File.ReadAllText(pathname)
    let outputPathname = $"tests/syntax/{name}_ast.txt"

    eprintfn "------------------------"
    eprintfn "file: %s" pathname

    let ast = parseString input
    File.WriteAllText(outputPathname, string ast)

    let asmPathname = $"tests/syntax/{name}.s"
    eprintfn "codeGen (%s)" asmPathname
    let code = codeGen ast
    File.WriteAllText(asmPathname, code)

    let exePathname = $"tests/syntax/{name}.exe"
    eprintfn "cc (%s)" exePathname
    let cc = "/usr/bin/gcc"
    let psi = ProcessStartInfo(cc)
    psi.ArgumentList.Add(asmPathname)
    psi.ArgumentList.Add("-o")
    psi.ArgumentList.Add(exePathname)
    Process.Start(psi).WaitForExit() |> ignore

  0
