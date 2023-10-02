// Pcc entrypoint.

// USAGE: dotnet run --project Pcc [FILES...]
//
// 入力ファイル (`*.simple`) をそれぞれパースして、
// 構文木 (`*_ast.txt`) とアセンブリ (`*.s`) を出力する

// MyYaccを使う機能:
// USAGE:
//    dotnet run --project Pcc --my-yacc <input.simple
//
// MyYaccを使って標準入力をパースする

module rec Pcc.Program

open System.Diagnostics
open System.IO
open System.Threading.Tasks
open Pcc.Parser
open Pcc.CodeGen

let inline private unreachable context = failwithf "unreachable: %A" context

let private trace msg = eprintfn "%s\n" msg

module private UseMyYacc =
  open MyYacc

  let private parse lexer input =
    let parser =
      let grammarText = File.ReadAllText("grammar.txt")
      generateParser grammarText

    let tokens = MyLex.NfaLexer.tokenize input lexer
    let root = parseTokensToTree (Array.ofList tokens) parser

    let root =
      // Rename root node.
      match root with
      | PElement.Node(name, children) ->
        eprintfn "  renaming root node '%s' -> Root" name
        PElement.Node("Root", children)
      | _ -> unreachable root

    AstLower.lowerRoot root

  let internal run () =
    task {
      let! lexText = File.ReadAllTextAsync(lexFile)
      let lexer = MyLex.NfaLexer.parse lexText
      let! input = stdin.ReadToEndAsync()
      let ast = parse lexer input
      do! stdout.WriteLineAsync(string ast)
    }
    |> (fun (t: Task<_>) -> t.GetAwaiter().GetResult())

    0

let private lexFile =
  System.Environment.GetEnvironmentVariable("LEX")
  |> Option.ofObj
  |> Option.defaultValue "lex.txt"

let private cc =
  System.Environment.GetEnvironmentVariable("CC")
  |> Option.ofObj
  |> Option.defaultValue "/usr/bin/gcc"

let private parseString lexer input =
  let tokens = MyLex.NfaLexer.tokenize input lexer
  parseTokens (Array.ofList tokens)

[<EntryPoint>]
let main _ =
  let args = System.Environment.GetCommandLineArgs()
  eprintfn "Pcc v0.0.0"
  eprintfn "  args = %A" args

  if args |> Array.contains "--my-yacc" then
    eprintfn "  using MyYacc"
    UseMyYacc.run () |> exit

  task {
    let! lexText = File.ReadAllTextAsync(lexFile)
    let lexer = MyLex.NfaLexer.parse lexText

    let! exitCodeArray =
      args
      |> Array.filter (fun pathname -> Path.GetExtension(pathname) = ".simple")
      |> Array.map (fun pathname ->
        task {
          eprintfn "------------------------"
          eprintfn "file: %s" pathname

          let outputPathname =
            Path.Combine(Path.GetDirectoryName(pathname), Path.GetFileNameWithoutExtension(pathname))
            + "_ast.txt"

          let asmPathname = Path.ChangeExtension(pathname, ".s")
          let exePathname = Path.ChangeExtension(pathname, ".exe")

          let! input = File.ReadAllTextAsync(pathname)

          let ast = parseString lexer input
          do! File.WriteAllTextAsync(outputPathname, string ast)

          let code = codeGen ast
          do! File.WriteAllTextAsync(asmPathname, code)

          let psi = ProcessStartInfo(cc)
          psi.ArgumentList.Add(asmPathname)
          psi.ArgumentList.Add("-o")
          psi.ArgumentList.Add(exePathname)
          let p = Process.Start(psi)
          do! p.WaitForExitAsync()
          return p.ExitCode
        })
      |> Task.WhenAll

    let ok = exitCodeArray |> Array.forall ((=) 0)
    return (if ok then 0 else 1)
  }
  |> (fun (t: Task<_>) -> t.GetAwaiter().GetResult())
