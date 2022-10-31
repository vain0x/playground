// 算術式のパーサの実装例
module internal ExArith

open Util

module P = ParserV2.ParserCombinator

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type internal Token =
  | Number of int
  | Plus
  | Minus
  | Star
  | Slash
  | LeftParen
  | RightParen

/// トークンの種類を表す適当な定数
module private TokenKind =
  let Number = 1
  let Plus = 2
  let Minus = 3
  let Star = 4
  let Slash = 5
  let LeftParen = 6
  let RightParen = 7

  let ofToken token =
    match token with
    | Token.Number _ -> Number
    | Token.Plus -> Plus
    | Token.Minus -> Minus
    | Token.Star -> Star
    | Token.Slash -> Slash
    | Token.LeftParen -> LeftParen
    | Token.RightParen -> RightParen

/// 二項演算子の種類
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type internal Binary =
  | Add
  | Subtract
  | Multiply
  | Divide

/// 数式の構文木
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type internal Expr =
  | Number of int
  | Paren of Expr
  | BinOp of Binary * Expr * Expr

let private pExpr: P.RecRule<_, Expr> = P.recursive "Expression"

/// カッコ式の規則 (`'(' E ')'`)
let private pParen =
  P.rule3 (P.cut TokenKind.LeftParen ignore) (P.recurse pExpr) (P.expect TokenKind.RightParen ignore) (fun _ e _ ->
    Expr.Paren e)

let private pPrimary =
  P.choice [ P.cut TokenKind.Number (fun (Token.Number value | Unreachable value) -> Expr.Number value)
             pParen ]

let private pMul =
  P.infixLeft
    pPrimary
    (P.choice [ P.cut TokenKind.Star (fun _ -> Binary.Multiply)
                P.cut TokenKind.Slash (fun _ -> Binary.Divide) ])
    pPrimary
    (fun l op r -> Expr.BinOp(op, l, r))

let private pAdd =
  P.infixLeft
    pMul
    (P.choice [ P.cut TokenKind.Plus (fun _ -> Binary.Add)
                P.cut TokenKind.Minus (fun _ -> Binary.Subtract) ])
    pMul
    (fun l op r -> Expr.BinOp(op, l, r))

let private sGrammarLazy: Lazy<P.Grammar<Token, Expr>> =
  lazy (P.build (P.recurse pExpr) [ P.bind pExpr pAdd ])

let private tokenize (text: string) : Token array =
  text
    .Replace("(", "( ")
    .Replace(")", " )")
    .Split(" ")
  |> Array.filter (fun s -> s <> "")
  |> Array.map (fun s ->
    match s with
    | "(" -> Token.LeftParen
    | ")" -> Token.RightParen
    | "+" -> Token.Plus
    | "-" -> Token.Minus
    | "*" -> Token.Star
    | "/" -> Token.Slash
    | _ -> Token.Number(int s))

let internal parseString (text: string) : Expr =
  let tokenArray = tokenize text
  P.parseV2 TokenKind.ofToken tokenArray sGrammarLazy.Value

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

  assert (p "42" "Number 42")
  assert (p "(42)" "Paren (Number 42)")
  assert (p "1 + 2" "BinOp (Add, Number 1, Number 2)")
  assert (p "2 * 3" "BinOp (Multiply, Number 2, Number 3)")
  assert (p "1 + 2 * 3" "BinOp (Add, Number 1, BinOp (Multiply, Number 2, Number 3))")
  assert (p "(1 + 2) * 3" "BinOp (Multiply, Paren (BinOp (Add, Number 1, Number 2)), Number 3)")

  assert (p "1 + 2 - 3" "BinOp (Subtract, BinOp (Add, Number 1, Number 2), Number 3)")

  assert
    (p
      "2 * 3 / 5 * 7"
      "BinOp\n  (Multiply, BinOp (Divide, BinOp (Multiply, Number 2, Number 3), Number 5),\n   Number 7)")
