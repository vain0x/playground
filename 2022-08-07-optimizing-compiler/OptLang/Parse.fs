module rec OptLang.Parse

open OptLang.Syntax
open OptLang.Tokenize

let private Tick = ref 0

// -----------------------------------------------
// Context
// -----------------------------------------------

/// Parser context.
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private Px =
  { Tokens: (Token * Pos * int) array
    mutable Index: int }

let private error msg (px: Px) =
  let i =
    if px.Index < px.Tokens.Length then
      px.Index
    else
      px.Tokens.Length - 1

  let token, pos, _ = px.Tokens.[i]
  failwithf "Parse error at %s (%A): %s" (Pos.toString pos) token msg

let private reset () = Tick.contents <- 0

let private incrementTick (px: Px) =
  Tick.contents <- Tick.contents + 1

  if Tick.contents >= 1_000_000 then
    error "Infinite loop" px

let private look (offset: int) (px: Px) : Token =
  assert (offset >= 0)

  incrementTick px

  if px.Index + offset < px.Tokens.Length then
    // trace "look: %d %A" (px.Index + offset) px.Tokens.[px.Index + offset]
    let token, _, _ = px.Tokens.[px.Index + offset]
    token
  else
    // EOFの代わり
    Token.Bad

let private atEof (px: Px) =
  incrementTick px

  px.Index = px.Tokens.Length

let private shift (px: Px) : Token =
  assert (px.Index < px.Tokens.Length)
  // trace "shift: %d %A" px.Index px.Tokens.[px.Index]

  let token, _, _ = px.Tokens.[px.Index]
  px.Index <- px.Index + 1
  token

let inline private bump (px: Px) : unit = shift px |> ignore

let inline private skip (px: Px) : unit = shift px |> ignore

let private canOmitSep (offset: int) (px: Px) =
  assert (offset >= 0)

  incrementTick px

  let i = px.Index + offset

  if 0 < i && i < px.Tokens.Length then
    let _, prevStart, _ = px.Tokens.[i - 1]
    let token, currentStart, _ = px.Tokens.[i]

    match token with
    | Token.RightParen
    | Token.RightBracket
    | Token.RightBrace -> true

    | _ -> Pos.y prevStart < Pos.y currentStart
  else
    i = 0

let private hasLeftSpace (offset: int) (px: Px) =
  assert (offset >= 0)

  incrementTick px

  let i = px.Index + offset

  if 0 < i && i < px.Tokens.Length then
    let _, prevStart, len = px.Tokens.[i - 1]
    let _, currentStart, _ = px.Tokens.[i]

    Pos.y prevStart < Pos.y currentStart
    || Pos.x prevStart + len < Pos.x currentStart
  else
    i = 0

let private hasRightSpace (offset: int) (px: Px) =
  assert (offset >= 0)

  incrementTick px

  let i = px.Index + offset

  if i + 1 < px.Tokens.Length then
    let _, currentStart, len = px.Tokens.[i]
    let _, nextStart, _ = px.Tokens.[i + 1]

    Pos.y currentStart < Pos.y nextStart
    || Pos.x currentStart + len < Pos.x nextStart
  else
    i = px.Tokens.Length - 1

// -----------------------------------------------
// Tokens
// -----------------------------------------------

let private expectPun (token: Token) (px: Px) =
  if look 0 px = token then
    bump px
  else
    error (sprintf "Expected '%A'" token) px

let private eatPun (token: Token) (px: Px) = if look 0 px = token then bump px

let private expectComma (px: Px) =
  if look 0 px = Token.Comma then
    bump px
  else if not (canOmitSep 0 px) then
    error "Expected a comma or newline" px

let private expectSemi (px: Px) =
  if look 0 px = Token.Semi then
    bump px
  else if not (canOmitSep 0 px) then
    error "Expected a semicolon or newline" px

// -----------------------------------------------
// Types
// -----------------------------------------------

let private parsePrimaryTy (px: Px) =
  match look 0 px with
  | Token.Ident ident ->
    bump px

    match ident with
    | "bool" -> Ty.Bool
    | "int" -> Ty.Int
    | "string" -> Ty.String

    | "array" ->
      expectPun Token.LeftParen px
      let itemTy = parseTy px
      expectPun Token.RightParen px
      Ty.Array itemTy

    | _ -> Ty.Name ident

  | Token.Void ->
    bump px
    Ty.Void

  | _ -> error "Expected type" px

let private parseTy px = parsePrimaryTy px

// -----------------------------------------------
// Expressions
// -----------------------------------------------

let private atExpr (px: Px) =
  match look 0 px with
  | Token.Int _
  | Token.String _
  | Token.Ident _
  | Token.LeftParen
  | Token.LeftBracket
  | Token.LeftBrace
  | Token.Bang
  | Token.Hyphen
  | Token.False
  | Token.True
  | Token.Void -> true
  | _ -> false

let private parseExpr (px: Px) : Expr = parseBinaryExpr 1 px

let private parsePrimaryExpr (px: Px) : Expr =
  let parseArgs (px: Px) =
    let rec nextArg acc px =
      match look 0 px with
      | Token.RightParen
      | Token.RightBracket -> List.rev acc

      | _ ->
        let itemExpr = parseExpr px
        expectComma px
        nextArg (itemExpr :: acc) px

    nextArg [] px

  match look 0 px with
  | Token.Int value ->
    bump px
    Expr.Int value

  | Token.String value ->
    bump px
    Expr.String value

  | Token.Ident name ->
    bump px

    match look 0 px with
    | Token.LeftParen when not (hasLeftSpace 0 px) ->
      bump px
      let args = parseArgs px
      expectPun Token.RightParen px
      Expr.Call(name, args)

    | _ -> Expr.Name name

  | Token.LeftParen ->
    bump px
    let expr = parseExpr px
    expectPun Token.RightParen px
    expr

  | Token.LeftBracket ->
    bump px
    let items = parseArgs px
    expectPun Token.RightBracket px
    Expr.Array items

  | Token.LeftBrace ->
    bump px

    let entries =
      let rec go acc (px: Px) =
        match look 0 px with
        | Token.RightBrace -> List.rev acc

        | Token.Ident field ->
          bump px
          expectPun Token.Colon px
          let init = parseExpr px
          expectComma px
          go ((field, init) :: acc) px

        | _ -> error "Expected a field" px

      go [] px

    expectPun Token.RightBrace px
    Expr.Record entries

  | Token.False ->
    bump px
    Expr.Name "false"

  | Token.True ->
    bump px
    Expr.Name "true"

  | Token.Void ->
    bump px
    Expr.Name "void"

  | _ -> error "Expected an expressions" px

let private parsePathExpr (px: Px) =
  let rec go lhs (px: Px) =
    match look 0 px with
    | Token.LeftBracket when not (hasLeftSpace 0 px) ->
      bump px
      let index = parseExpr px
      expectPun Token.RightBracket px
      go (Expr.Index(lhs, index)) px

    | Token.Dot when not (hasRightSpace 0 px) ->
      bump px

      match look 0 px with
      | Token.Ident field ->
        bump px
        go (Expr.Field(lhs, field)) px

      | _ -> error "Expected a field" px

    | _ -> lhs

  let expr = parsePrimaryExpr px
  go expr px

let private parsePrefixExpr (px: Px) =
  match look 0 px with
  | Token.Bang ->
    bump px

    let arg = parsePathExpr px
    Expr.Call("not", [ arg ])

  | Token.Hyphen ->
    bump px

    let arg = parsePathExpr px
    Expr.Call("minus", [ arg ])

  | _ -> parsePathExpr px

let private parseBinaryExpr level (px: Px) : Expr =
  let parseArg (px: Px) =
    if level = 5 then
      parsePrefixExpr px
    else
      parseBinaryExpr (level + 1) px

  let rec go lExpr (px: Px) =
    let op =
      match level, look 0 px with
      | 1, Token.PipePipe -> "||"
      | 2, Token.AmpAmp -> "&&"
      | 3, Token.EqualEqual -> "=="
      | 3, Token.BangEqual -> "!="
      | 3, Token.LeftAngle -> "<"
      | 3, Token.LeftEqual -> "<="
      | 3, Token.RightAngle -> ">"
      | 3, Token.RightEqual -> ">="
      | 4, Token.Plus -> "+"
      | 4, Token.Hyphen -> "-"
      | 5, Token.Star -> "*"
      | 5, Token.Slash -> "/"
      | 5, Token.Percent -> "%"
      | _ -> ""

    if op <> "" then
      bump px
      let rExpr = parseArg px
      go (Expr.Call(op, [ lExpr; rExpr ])) px
    else
      lExpr

  let expr = parseArg px
  go expr px

// -----------------------------------------------
// Statements
// -----------------------------------------------

let private atStmt (px: Px) =
  match look 0 px with
  | Token.Break
  | Token.Continue
  | Token.If
  | Token.Let
  | Token.Loop
  | Token.Return
  | Token.While -> true
  | _ -> atExpr px

let private parseStmt (px: Px) : Stmt =
  match look 0 px with
  | Token.Break ->
    bump px
    expectSemi px
    Stmt.Break

  | Token.Continue ->
    bump px
    expectSemi px
    Stmt.Continue

  | Token.If ->
    bump px
    let cond = parseExpr px
    let body = parseBlock px
    expectPun Token.Else px

    let alt =
      match look 0 px with
      | Token.If -> parseStmt px
      | _ -> Stmt.Block(parseBlock px)

    Stmt.If(cond, Stmt.Block body, alt)

  | Token.Let ->
    bump px

    let name =
      match look 0 px with
      | Token.Ident it ->
        bump px
        it

      | _ -> error "Expected an identifier" px

    let tyOpt =
      match look 0 px with
      | Token.Colon ->
        bump px
        parseTy px |> Some

      | _ -> None

    expectPun Token.Equal px

    let init = parseExpr px
    expectSemi px

    Stmt.Let(name, tyOpt, init)

  | Token.Loop ->
    bump px
    let block = parseBlock px
    Stmt.Loop(Stmt.Block block)

  | Token.Return ->
    bump px

    let argOpt =
      if atExpr px then
        parseExpr px |> Some
      else
        None

    expectSemi px
    Stmt.Return argOpt

  | Token.While ->
    bump px
    let cond = parseExpr px
    let body = parseBlock px

    Stmt.If(cond, Stmt.Block body, Stmt.Break)
    |> Stmt.Loop

  | _ ->
    let expr = parseExpr px

    match look 0 px with
    | Token.Equal ->
      bump px
      let rhs = parseExpr px
      expectSemi px
      Stmt.Assign(expr, rhs)

    | _ ->
      expectSemi px
      Stmt.Do expr

let private parseBlock (px: Px) : Block =
  expectPun Token.LeftBrace px

  let rec go acc (px: Px) =
    if atStmt px then
      let stmt = parseStmt px
      go (stmt :: acc) px
    else
      List.rev acc

  let stmts = go [] px

  expectPun Token.RightBrace px
  { Locals = []; Stmts = stmts }

// -----------------------------------------------
// Declarations
// -----------------------------------------------

let private parseDecl (px: Px) : Decl =
  match look 0 px with
  | Token.LeftBrace -> Decl.Block(parseBlock px)

  | Token.Fn ->
    bump px

    let name =
      match look 0 px with
      | Token.Ident name ->
        bump px
        name

      | _ -> error "Expected a function name" px

    expectPun Token.LeftParen px

    let paramList =
      let rec go acc (px: Px) =
        match look 0 px with
        | Token.RightParen -> List.rev acc

        | Token.Ident name ->
          bump px
          expectPun Token.Colon px
          let ty = parseTy px
          expectComma px
          go ((name, ty) :: acc) px

        | _ -> error "Expected a parameter" px

      go [] px

    expectPun Token.RightParen px
    expectPun Token.Arrow px
    let resultTy = parseTy px

    let body = parseBlock px

    Decl.Fn(name, paramList, resultTy, Stmt.Block body)

  | Token.Type ->
    bump px

    let name =
      match look 0 px with
      | Token.Ident name ->
        bump px
        name

      | _ -> error "Expected a type name" px

    expectPun Token.LeftBrace px

    let fields =
      let rec go acc (px: Px) =
        match look 0 px with
        | Token.RightBrace -> List.rev acc

        | Token.Ident field ->
          bump px
          expectPun Token.Colon px
          let ty = parseTy px
          expectComma px
          go ((field, ty) :: acc) px

        | _ -> error "Expected a field" px

      go [] px

    expectPun Token.RightBrace px
    Decl.RecordTy(name, fields)

  | _ -> error "Expected a declaration" px

// -----------------------------------------------
// Interface
// -----------------------------------------------

let parseTokens tokens =
  let mutable px: Px = { Tokens = tokens; Index = 0 }

  let rec go acc (px: Px) =
    if not (atEof px) then
      let decl = parseDecl px
      go (decl :: acc) px
    else
      List.rev acc

  go [] px
