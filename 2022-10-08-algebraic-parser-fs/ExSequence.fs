/// 数列のパーサの実装例
module internal ExNumberSequence

open Util

module P = ParserV2.ParserCombinator

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type internal Token =
  | Number of int
  | Comma

/// トークンの種類を表す適当な定数
module private TokenKind =
  let Number = 1
  let Comma = 2

  let ofToken token =
    match token with
    | Token.Number _ -> Number
    | Token.Comma -> Comma

let private pNumber =
  P.expect TokenKind.Number (fun (Token.Number value | Unreachable value) -> value)

let private pSeq =
  P.opt
    (P.rule2
      pNumber
      // (, n)*
      (P.rep (P.rule2 (P.expect TokenKind.Comma ignore) pNumber (fun _ n -> n)))
      (fun head tail -> head :: tail))
    (fun opt ->
      match opt with
      | Some it -> it
      | None -> [])

let private sGrammarLazy: Lazy<P.Grammar<Token, int list>> = lazy (P.build pSeq [])

let private tokenize (text: string) : Token array =
  let array = ResizeArray()
  let mutable i = 0

  for word in text.Split(",") do
    let word = word.Trim()

    if word <> "" then
      if i <> 0 then array.Add(Token.Comma)

      array.Add(Token.Number(int word))
      i <- i + 1

  array.ToArray()

let internal parseString (text: string) : int list =
  let tokenArray = tokenize text
  P.parseArray TokenKind.ofToken tokenArray sGrammarLazy.Value

let internal tests () =
  let p s x =
    let actual =
      try
        parseString s |> sprintf "%A"
      with
      | ex -> sprintf "ERROR: %A" ex

    if actual = x then
      true
    else
      eprintfn "error: Assertion violated:\nactual: '%s'\nexpected: '%s'" actual x
      false

  assert (p "" "[]")
  assert (p "0" "[0]")
  assert (p "0, 1, 2" "[0; 1; 2]")
