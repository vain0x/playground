/// ミニ言語のパーサー
module internal ExMiniLang

open Util

module P = ParserV2.ParserCombinator

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type internal Token =
  | Bad
  | Spaces
  | Number of int
  | Ident of string
  // Keywords:
  | Break
  | Continue
  | Else
  | Enum
  | Fn
  | For
  | If
  | Let
  | Loop
  | Match
  | Return
  | Struct
  | Underscore
  | While
  // Brackets:
  | LeftParen
  | RightParen
  | LeftBracket
  | RightBracket
  | LeftBrace
  | RightBrace
  // Sigils:
  | Bang
  | BangEqual
  | Colon
  | Comma
  | Equal
  | EqualEqual
  | LeftAngle
  | Minus
  | MinusEqual
  | Percent
  | Plus
  | PlusEqual
  | RightFatArrow
  | Semi
  | Slash
  | Star

  | False
  | True
  | Dot
  | RightThinArrow

/// トークンの種類を表す適当な定数
module private TokenKind =
  let Bad = 1
  let Number = 2
  let Ident = 3
  // Keywords:
  let Break = 4
  let Continue = 5
  let Else = 6
  let Enum = 7
  let Fn = 8
  let For = 9
  let If = 10
  let Let = 11
  let Loop = 12
  let Match = 13
  let Return = 14
  let Struct = 15
  let Underscore = 16
  let While = 17
  // Brackets:
  let LeftParen = 18
  let RightParen = 19
  let LeftBracket = 20
  let RightBracket = 21
  let LeftBrace = 22
  let RightBrace = 23
  // Sigils:
  let Bang = 24
  let BangEqual = 25
  let Colon = 26
  let Comma = 27
  let Equal = 28
  let EqualEqual = 29
  let LeftAngle = 30
  let Minus = 31
  let MinusEqual = 32
  let Percent = 33
  let Plus = 34
  let PlusEqual = 35
  let RightFatArrow = 36
  let Semi = 37
  let Slash = 38
  let Star = 39

  let False = 40
  let True = 41
  let Dot = 42
  let Spaces = 43
  let RightThinArrow = 44

  let ofToken token =
    match token with
    | Token.Bad -> Bad
    | Token.Number _ -> Number
    | Token.Ident _ -> Ident
    // Keywords:
    | Token.Break -> Break
    | Token.Continue -> Continue
    | Token.Else -> Else
    | Token.Enum -> Enum
    | Token.Fn -> Fn
    | Token.For -> For
    | Token.If -> If
    | Token.Let -> Let
    | Token.Loop -> Loop
    | Token.Match -> Match
    | Token.Return -> Return
    | Token.Struct -> Struct
    | Token.Underscore -> Underscore
    | Token.While -> While
    // Brackets:
    | Token.LeftParen -> LeftParen
    | Token.RightParen -> RightParen
    | Token.LeftBracket -> LeftBracket
    | Token.RightBracket -> RightBracket
    | Token.LeftBrace -> LeftBrace
    | Token.RightBrace -> RightBrace
    // Sigils:
    | Token.Bang -> Bang
    | Token.BangEqual -> BangEqual
    | Token.Colon -> Colon
    | Token.Comma -> Comma
    | Token.Equal -> Equal
    | Token.EqualEqual -> EqualEqual
    | Token.LeftAngle -> LeftAngle
    | Token.Minus -> Minus
    | Token.MinusEqual -> MinusEqual
    | Token.Percent -> Percent
    | Token.Plus -> Plus
    | Token.PlusEqual -> PlusEqual
    | Token.RightFatArrow -> RightFatArrow
    | Token.Semi -> Semi
    | Token.Slash -> Slash
    | Token.Star -> Star

    | Token.False -> False
    | Token.True -> True
    | Token.Dot -> Dot
    | Token.Spaces -> Spaces
    | Token.RightThinArrow -> RightThinArrow

  let internal asKeyword s =
    match s with
    | "break" -> Token.Break
    | "continue" -> Token.Continue
    | "else" -> Token.Else
    | "enum" -> Token.Enum
    | "false" -> Token.False
    | "fn" -> Token.Fn
    | "for" -> Token.For
    | "if" -> Token.If
    | "let" -> Token.Let
    | "loop" -> Token.Loop
    | "match" -> Token.Match
    | "return" -> Token.Return
    | "struct" -> Token.Struct
    | "true" -> Token.True
    | "underscore" -> Token.Underscore
    | "while" -> Token.While
    | _ -> Token.Ident s

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type internal Unary =
  | Minus
  | Not

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type internal Binary =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Equal
  | NotEqual
  | LessThan

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type internal BinaryAssign =
  | Assign
  | AddAssign
  | SubtractAssign

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type internal Literal =
  | Unit
  | Bool of bool
  | Int of int

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type internal Ty =
  | Ident of string
  | Tuple of Ty list

and [<RequireQualifiedAccess; NoEquality; NoComparison>] internal Pat =
  | Ident of string
  | Literal of Literal
  | Or of Pat * Pat

and [<RequireQualifiedAccess; NoEquality; NoComparison>] internal Expr =
  | Ident of string
  | Literal of Literal
  | Tuple of Expr * Expr * Expr list
  | Call of Expr * Expr list
  | Index of Expr * Expr
  | Field of Expr * string
  | UniOp of Unary * Expr
  | BinOp of Binary * Expr * Expr
  | Assign of BinaryAssign * Expr * Expr
  | Block of Block
  | Break
  | Continue
  | Return of Expr option
  | If of cond: Expr * thenClause: Expr * elseClause: Expr option
  | Loop of Block

and [<RequireQualifiedAccess; NoEquality; NoComparison>] internal Stmt =
  | Expr of Expr
  | Let of Pat * Expr

and [<RequireQualifiedAccess; NoEquality; NoComparison>] internal Item =
  | FnItem of FnItem
  | EnumItem of EnumItem
  | StructItem of StructItem

and [<RequireQualifiedAccess; NoEquality; NoComparison>] internal FnItem =
  { Name: string
    Params: (string * Ty) list
    ResultOpt: Ty option
    Body: Block }

and [<RequireQualifiedAccess; NoEquality; NoComparison>] internal EnumItem =
  { Name: string
    Variants: (string * Ty option) list }

and [<RequireQualifiedAccess; NoEquality; NoComparison>] internal StructItem =
  { Name: string
    Fields: (string * Ty) list }

and [<RequireQualifiedAccess; NoEquality; NoComparison>] internal Block =
  { Items: Item list
    Stmts: Stmt list
    Expr: Expr option }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type internal Root = { Items: Item list }

// 1+ items separated by sep and may dangle sep
let private by1 pItem pSep =
  let wrap p = P.rule1 p List.singleton

  P.rule2 (P.infixLeft (wrap pItem) pSep (wrap pItem) (fun l _ r -> l @ r)) (P.opt pSep ignore) (fun items _ -> items)

// 0+ items separated by sep and may dangle sep unless empty
let private by0 pItem pSep =
  P.opt (by1 pItem pSep) (Option.defaultValue [])

let private pTyRec: P.RecRule<_, Ty> = P.recursive "Type"
// let private pPatRec: P.RecRule<_, Pat> = P.recursive "Pattern"
let private pExprRec: P.RecRule<_, Expr> = P.recursive "Expression"
let private pStmtRec: P.RecRule<_, Stmt> = P.recursive "Statement"
let private pItemRec: P.RecRule<_, Item> = P.recursive "Item"

let private pRecTy = P.recurse pTyRec
// let private pRecPat = P.recurse pPatRec
let private pRecExpr = P.recurse pExprRec
let private pRecStmt = P.recurse pStmtRec
let private pRecItem = P.recurse pItemRec

let private lp = P.expect TokenKind.LeftParen ignore
let private rp = P.expect TokenKind.RightParen ignore
let private lb = P.expect TokenKind.LeftBracket ignore
let private rb = P.expect TokenKind.RightBracket ignore
let private lc = P.expect TokenKind.LeftBrace ignore
let private rc = P.expect TokenKind.RightBrace ignore
let private colon = P.expect TokenKind.Colon ignore
let private comma = P.expect TokenKind.Comma ignore
let private semi = P.expect TokenKind.Semi ignore

let private pIdent =
  P.expect TokenKind.Ident (fun (Token.Ident it | Unreachable it) -> it)

let private pLiteral =
  P.choice [ // P.rule2 lp rp (fun _ _ -> Literal.Unit)

             P.expect TokenKind.Number (fun (Token.Number value | Unreachable value) -> Literal.Int value)
             P.expect TokenKind.False (fun _ -> Literal.Bool false)
             P.expect TokenKind.True (fun _ -> Literal.Bool true) ]

// `Stmt* Expr` をパースする。ただしこれは曖昧なので以下の構文に変換する
// `μX. Expr (; X)? | Stmt X`
let private pBlockItems: P.Rule<_, Stmt list * Expr option> =
  P.mu "BlockItems" (fun pSelf ->
    P.choice [ P.rule2 pRecExpr (P.opt (P.rule2 semi pSelf (fun _ result -> result)) id) (fun expr restOpt ->
                 match restOpt with
                 | Some (stmts, exprOpt) -> Stmt.Expr expr :: stmts, exprOpt
                 | None -> [], Some expr)
               P.rule2 pRecStmt pSelf (fun stmt (stmts, exprOpt) -> stmt :: stmts, exprOpt) ])

let private pBlock =
  P.rule3 lc pBlockItems rc (fun _ (stmts, exprOpt) _ ->
    ({ Items = []
       Stmts = stmts
       Expr = exprOpt }: Block))

let private pTy1 =
  P.choice [ P.rule1 pIdent Ty.Ident

             //  P.rule2 lp rp (fun _ _ -> Ty.Tuple [])
             //  P.rule3 lp pRecTy rp (fun _ it _ -> it)

             P.rule2
               lp
               (P.choice [ P.rule1 rp (fun _ -> Ty.Tuple [])
                           P.rule2 pRecTy rp (fun it _ -> it) ])
               (fun _ it -> it) ]

let private pPrimary =
  P.choice [ P.rule1 pLiteral Expr.Literal
             P.rule1 pIdent Expr.Ident

             // P.rule3 lp pRecExpr rp (fun _ e _ -> e)

             P.rule2
               lp
               (P.choice [ P.rule1 rp (fun _ -> Expr.Literal Literal.Unit)
                           P.rule2 pRecExpr rp (fun e _ -> e) ])
               (fun _ it -> it) ]

let private pSuffix =
  P.infixLeft
    pPrimary
    (P.choice [ P.rule2 (P.expect TokenKind.Dot ignore) pIdent (fun _ field lhs -> Expr.Field(lhs, field))
                P.rule3 lb pRecExpr rb (fun _ index _ lhs -> Expr.Index(lhs, index)) ])
    (P.eps ())
    (fun lhs mid _ -> mid lhs)

let private pPrefix =
  P.choice [ P.rule2
               (P.choice [ P.expect TokenKind.Minus (fun _ -> Unary.Minus)
                           P.expect TokenKind.Bang (fun _ -> Unary.Not) ])
               pSuffix
               (fun op arg -> Expr.UniOp(op, arg))
             pSuffix ]

let private pMul =
  P.infixLeft
    pPrefix
    (P.choice [ P.cut TokenKind.Star (fun _ -> Binary.Multiply)
                P.cut TokenKind.Slash (fun _ -> Binary.Divide) ])
    pPrefix
    (fun l op r -> Expr.BinOp(op, l, r))

let private pAdd =
  P.infixLeft
    pMul
    (P.choice [ P.cut TokenKind.Plus (fun _ -> Binary.Add)
                P.cut TokenKind.Minus (fun _ -> Binary.Subtract) ])
    pMul
    (fun l op r -> Expr.BinOp(op, l, r))

let private pAssign =
  P.rule2 pAdd (P.opt (P.rule2 (P.expect TokenKind.Equal ignore) pRecExpr (fun _ e -> e)) id) (fun l rOpt ->
    match rOpt with
    | Some r -> Expr.Assign(BinaryAssign.Assign, l, r)
    | None -> l)

let private pIfExpr =
  P.rule4
    (P.expect TokenKind.If ignore)
    pRecExpr
    pBlock
    (P.opt (P.rule2 (P.expect TokenKind.Else ignore) pBlock (fun _ a -> Expr.Block a)) id)
    (fun _ c b a -> Expr.If(c, Expr.Block b, a))

let private pExpr1 = P.choice [ pAssign; pIfExpr ]

let private pStmt1 =
  let pExprStmt = P.rule2 pRecExpr semi (fun e _ -> Stmt.Expr e)

  // let ;
  let pLetStmt =
    P.rule2 (P.expect TokenKind.Let ignore) semi (fun _ _ ->
      Stmt.Let(Pat.Literal Literal.Unit, Expr.Literal Literal.Unit))

  P.choice [ pExprStmt; pLetStmt ]

let private pParamList =
  let pParam = P.rule3 pIdent colon pRecTy (fun name _ ty -> name, ty)

  P.rule3 lp (by0 pParam comma) rp (fun _ it _ -> it)

let private pItem1 =
  P.rule5
    (P.expect TokenKind.Fn ignore)
    pIdent
    pParamList
    (P.opt (P.rule2 (P.expect TokenKind.RightThinArrow ignore) pRecTy (fun _ ty -> ty)) id)
    pBlock
    (fun _ name paramList resultOpt body ->
      ({ Name = name
         Params = paramList
         ResultOpt = resultOpt
         Body = body }: FnItem))
  |> (fun p -> P.rule1 p Item.FnItem)

let private pRoot =
  P.rule1 (P.rep pRecItem) (fun items -> ({ Items = items }: Root))

let private sGrammarLazy: Lazy<P.Grammar<Token, Root>> =
  lazy
    (P.build
      pRoot
      [ P.bind pTyRec pTy1

        // P.bind pPatRec pRecPat

        P.bind pExprRec pExpr1
        P.bind pStmtRec pStmt1
        P.bind pItemRec pItem1 ])

module internal Tokenizer =
  let private punctuations =
    [| "(", Token.LeftParen
       ")", Token.RightParen
       "[", Token.LeftBracket
       "]", Token.RightBracket
       "{", Token.LeftBrace
       "}", Token.RightBrace

       "!", Token.Bang
       "!=", Token.BangEqual
       ":", Token.Colon
       ",", Token.Comma
       "=", Token.Equal
       "==", Token.EqualEqual
       "<", Token.LeftAngle
       "-", Token.Minus
       "-=", Token.MinusEqual
       "%", Token.Percent
       "+", Token.Plus
       "+=", Token.PlusEqual
       "=>", Token.RightFatArrow
       ";", Token.Semi
       "/", Token.Slash
       "*", Token.Star

       ".", Token.Dot
       "->", Token.RightThinArrow |]
    |> Array.sortByDescending fst

  let private isDigit (c: char) = '0' <= c && c <= '9'
  let private isUpper (c: char) = 'A' <= c && c <= 'Z'
  let private isLower (c: char) = 'a' <= c && c <= 'z'
  let private isLetter c = isUpper c || isLower c || c = '_'
  let private isSpace c = c = ' ' || c = '\r' || c = '\n'

  let internal tokenize (s: string) =
    let mutable tokens = ResizeArray()
    let mutable index = 0

    while index < s.Length do
      if isSpace s.[index] then
        index <- index + 1
      else if isLetter s.[index] then
        let start = index

        while index < s.Length
              && (isLetter s.[index] || isDigit s.[index]) do
          index <- index + 1

        let token = TokenKind.asKeyword s.[start .. index - 1]
        tokens.Add(token)
      else if isDigit s.[index] then
        let start = index

        while index < s.Length && isDigit s.[index] do
          index <- index + 1

        tokens.Add(Token.Number(int s.[start .. index - 1]))
      else
        match punctuations
              |> Array.tryFindIndex (fun (p, _) -> s.[index .. index + p.Length - 1] = p)
          with
        | Some i ->
          let p, t = punctuations.[i]
          index <- index + p.Length
          tokens.Add(t)

        | None -> failwithf "Unrecognized character %A at %d" s.[index] index

    tokens.ToArray()

let internal parseString (text: string) : Root =
  let tokenArray = Tokenizer.tokenize text
  P.parseV2 TokenKind.ofToken tokenArray sGrammarLazy.Value

let internal tests () =
  let display (root: Root) = sprintf "%A" root

  let p s expected =
    let actual = parseString s |> display

    if actual = expected then
      true
    else
      eprintfn "  actual: %s\n  expected: %s" actual expected
      false

  // assert (p "fn f(x: int, y: string) -> () { 0; let; 1 + 2 * 3 }" "Unit")
  let s = "fn f(x: int, y: string) -> () { 0; let; 1 + 2 * 3 }"
  s |> parseString |> display |> eprintfn "%s"
