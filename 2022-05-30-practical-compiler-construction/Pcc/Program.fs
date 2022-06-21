module rec Pcc.Program

open System.IO

[<EntryPoint>]
let main _ =
  let args = System.Environment.GetCommandLineArgs()
  let lexText = File.ReadAllText(args.[1])
  let grammarText = File.ReadAllText(args.[2])
  let input = stdin.ReadToEnd()

  // let rules =
  //   try
  //     MyLex.parseLexer input
  //   with
  //   | MyLex.ParseLexerException (msg, row, column) ->
  //     printfn "PARSE ERROR: %s at %d:%d" msg (row + 1) (column + 1)
  //     exit 1

  // // printfn "%A" rules
  // rules
  // |> List.iter (fun (name, term) ->
  //   if name = "STR" || name = "ID" then
  //     printfn "%s: %A" name term)

  // let nfa = MyLex.generateNfa rules

  // let u, trans, accepts = nfa
  // printfn "%d\n%A\n%A" u trans accepts

  // let em (input: string) =
  //   printf "input: %A    -> " input
  //   let result = MyLex.emulateNfa input nfa
  //   printfn "%A" result

  // em "42"
  // // em "-123"
  // em "a"
  // em "_Az09"
  // em "if"
  // em "iff"
  // em "\"\""
  // em "\"...\""
  // em "\"Hello, world!\""
  // em ";"
  // em "<="

  // let tok (input: string) =
  //   printfn "tokenize: %A" input

  //   let result =
  //     try
  //       MyLex.tokenizeWithNfa input nfa
  //     with
  //     | MyLex.TokenizeException index ->
  //       printfn "ERROR: Tokenize failed at %d\n" index
  //       printfn "  %s" input
  //       printfn "  %s^" (String.replicate index " ")
  //       exit 1

  //   result
  //   |> List.map (fun (kind, len) -> sprintf "%s(%d)" kind len)
  //   |> String.concat " "
  //   |> printfn "  %s"

  // tok "a"
  // tok "if"
  // tok "if (x == 0) { sprint \"Hello, world!\"; }"

  // try
  //   MyYacc.dump input
  // with
  // | MyYacc.ParseGrammarException (msg, i) -> eprintfn "ERROR: %s at %d" msg i

  let parser = MyYacc.generateLrParser grammarText
  let tokens = input.Trim().Split(' ')
  let events = MyYacc.LrParser.parse (Array.toList tokens) parser

  let rec go indent count events =
    (match events with
     | _ when count = 0 -> events

     | MyYacc.ParseEvent.Token i :: events ->
       eprintfn "%s%s %s" indent "token" tokens.[i]
       go indent (count - 1) events

     | MyYacc.ParseEvent.StartNode (name, childrenCount) :: events ->
       eprintfn "%s%s %s (%d)" indent "node" name childrenCount
       let events = go (indent + "  ") childrenCount events
       go indent (count - 1) events

     | [] -> failwith "unreachable")

  let events = go "" 1 events
  eprintfn "rest: %A" events

  0
