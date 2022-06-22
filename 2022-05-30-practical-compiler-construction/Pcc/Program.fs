module rec Pcc.Program

open System.IO

/// Parse tree element.
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type PElement =
  | Token of name: string * text: string
  | Node of name: string * children: PElement list

[<EntryPoint>]
let main _ =
  let args = System.Environment.GetCommandLineArgs()
  let lexText = File.ReadAllText(args.[1])
  let grammarText = File.ReadAllText(args.[2])
  let input = stdin.ReadToEnd()

  // Generate lexer.
  let rules =
    try
      MyLex.parseLexer lexText
    with
    | MyLex.ParseLexerException (msg, row, column) ->
      printfn "FATAL: Invalid lexer. %s at %d:%d" msg (row + 1) (column + 1)
      exit 1

  let nfa = MyLex.generateNfa rules

  // Generate parser.
  let parser =
    try
      MyYacc.generateLrParser grammarText
    with
    | MyYacc.ParseGrammarException (msg, i) ->
      eprintfn "FATAL: Invalid grammar. %s at %d" msg i
      exit 1

  // let dumpGrammar () = MyYacc.dump grammarText

  // Tokenize.
  let tokens =
    try
      MyLex.tokenizeWithNfa input nfa
    with
    | MyLex.TokenizeException index ->
      printfn "ERROR: Tokenize failed at %d\n" index
      printfn "  %s" input
      printfn "  %s^" (String.replicate index " ")
      exit 1

  let tokens =
    let t = ResizeArray()
    let mutable cursor = 0

    for kind, len in tokens do
      if kind <> "SPACE" then
        t.Add(kind, input.[cursor .. cursor + len - 1])

      cursor <- cursor + len

    t.ToArray()

  // let dumpTokens () =
  //   tokens
  //   |> List.map (fun (kind, len) -> sprintf "%s(%d)" kind len)
  //   |> String.concat " "
  //   |> printfn "  %s"

  // Parse.
  let events =
    let tokens = tokens |> Array.map fst |> Array.toList
    MyYacc.LrParser.parse tokens parser

  let dumpParseEvents events =
    let rec go indent count events =
      match events with
      | _ when count = 0 -> events

      | MyYacc.ParseEvent.Token i :: events ->
        eprintfn "%s%s %s" indent "token" (fst tokens.[i])
        go indent (count - 1) events

      | MyYacc.ParseEvent.StartNode (name, childrenCount) :: events ->
        eprintfn "%s%s %s (%d)" indent "node" name childrenCount
        let events = go (indent + "  ") childrenCount events
        go indent (count - 1) events

      | [] -> failwith "unreachable"

    let events = go "" 1 events
    eprintfn "rest: %A" events

  let root =
    let rec go acc count events =
      match events with
      | _ when count = 0 -> List.rev acc, events

      | MyYacc.ParseEvent.Token i :: events ->
        let kind, text = tokens.[i]
        go (PElement.Token(kind, text) :: acc) (count - 1) events

      | MyYacc.ParseEvent.StartNode (name, childrenCount) :: events ->
        let children, events = go [] childrenCount events
        go (PElement.Node(name, children) :: acc) (count - 1) events

      | [] -> failwith "unreachable"

    go [] 1 events

  eprintfn "%A" root
  0
