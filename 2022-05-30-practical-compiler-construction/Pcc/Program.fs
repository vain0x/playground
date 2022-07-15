module rec Pcc.Program

open System.Diagnostics
open System.IO
open System.Threading.Tasks
open Pcc.Parser
open Pcc.CodeGen

let inline private unreachable context = failwithf "unreachable: %A" context

let private trace msg = eprintfn "%s\n" msg

let private lexFile =
  System.Environment.GetEnvironmentVariable("LEX")
  |> Option.ofObj
  |> Option.defaultValue "lex.txt"

let private cc =
  System.Environment.GetEnvironmentVariable("CC")
  |> Option.ofObj
  |> Option.defaultValue "/usr/bin/gcc"

let private parseString lexer input =
  // let grammarText = File.ReadAllText("grammar.txt")
  // let parser = MyYacc.generateParser grammarText
  // let root = MyYacc.parseTokensToTree (Array.ofList tokens) parser
  // AstLower.lowerRoot root

  let tokens = MyLex.NfaLexer.tokenize input lexer
  parseTokens (Array.ofList tokens)

[<EntryPoint>]
let main _ =
  let args = System.Environment.GetCommandLineArgs()

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
