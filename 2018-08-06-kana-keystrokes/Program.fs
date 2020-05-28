module Program

open System

let rules = [
  "A", "あ", ""
  "I", "い", ""
  "U", "う", ""
  "E", "え", ""
  "O", "お", ""
  "YA", "や", ""
  "YI", "い", ""
  "YU", "ゆ", ""
  "YE", "いぇ", ""
  "YO", "よ", ""
  "XYA", "ゃ", ""
  "XYU", "ゅ", ""
  "XYO", "ょ", ""
  "TT", "っ", "T"
  "TA", "た", ""
  "TI", "ち", ""
  "TU", "つ", ""
  "TE", "て", ""
  "TO", "と", ""
  "TYA", "ちゃ", ""
  "TYU", "ちゅ", ""
  "TYO", "ちょ", ""
  "TSU", "つ", ""
  "CC", "っ", "C"
  "CHA", "ちゃ", ""
  "XTU", "っ", ""
  "XTSU", "っ", ""
]

let rec reduceComplete (source: string) (input: string) =
  seq {
    let mutable leaf = true
    if source.Length > 0 && input.Length > 0 then
      for given, target, remain in rules do
        if input.StartsWith(given) && source.StartsWith(target) then
          leaf <- false
          yield! reduceComplete (source.Substring(target.Length)) (remain + input.Substring(given.Length))
    if leaf then
      yield source, input
  }

let rec findNext (source: string) (input: string) =
  seq {
    for given, target, _ in rules do
      if given.StartsWith(input) && source.StartsWith(target) then
        yield given, target
  }

[<EntryPoint>]
let main _ =
  let source = "つっちゃった"

  let rec go () =
    eprintf "Input: "
    match Console.ReadLine() with
    | null -> ()
    | input ->
      let input = input.ToUpperInvariant()

      for source, input in reduceComplete source input do
        printfn "(%s, %s)" source input
        for g, t in findNext source input do
          printfn "    (%s, %s)" g t

      go ()

  go ()
  0
