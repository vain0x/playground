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

  // printfn "%A" rules
  rules
  |> List.iter (fun (name, term) ->
    if name = "STR" || name = "ID" then
      printfn "%s: %A" name term)

  let nfa = MyLex.generateNfa rules

  let u, trans, accepts = nfa
  printfn "%d\n%A\n%A" u trans accepts

  let em (input: string) =
    printf "input: %A    -> " input
    let result = MyLex.emulateNfa input nfa
    printfn "%A" result

  em "42"
  // em "-123"
  em "a"
  em "_Az09"
  em "if"
  em "iff"
  em "\"\""
  em "\"...\""
  em "\"Hello, world!\""
  em ";"
  em "<="
  0
