module rec Pcc.Program

[<EntryPoint>]
let main _ =
  let input = stdin.ReadToEnd()

  let rules =
    try
      MyLex.parseLexer input
    with
    | MyLex.ParseLexerException (msg, row, column) ->
      printfn "PARSE ERROR: %s at %d:%d" msg (row + 1) (column + 1)
      exit 1

  printfn "%A" rules
  0
