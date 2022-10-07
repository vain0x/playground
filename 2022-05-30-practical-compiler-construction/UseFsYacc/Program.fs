module Program

open FSharp.Text.Lexing

[<EntryPoint>]
let main _ =
  let input = stdin.ReadToEnd()

  let lexbuf = LexBuffer<_>.FromString input
  let ast = Parser.prog Lexer.read lexbuf
  printfn "%A" ast
  0
