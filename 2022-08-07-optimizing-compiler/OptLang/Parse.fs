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
  { Tokens: (Token * Pos) array
    mutable Index: int }

let private error msg (px: Px) =
  let i =
    if px.Index < px.Tokens.Length then
      px.Index
    else
      px.Tokens.Length - 1

  let token, pos = px.Tokens.[i]
  failwithf "Parse error at %s (%A): %s" (Pos.toString pos) token msg

let private reset () = Tick.contents <- 0

let private look (offset: int) (px: Px) : Token =
  assert (offset >= 0)

  incr Tick

  if Tick.contents >= 1000000 then
    error "Infinite loop" px

  if px.Index + offset < px.Tokens.Length then
    // trace "look: %d %A" (px.Index + offset) px.Tokens.[px.Index + offset]
    fst px.Tokens.[px.Index + offset]
  else
    // EOFの代わり
    Token.Bad

let inline private atEof (px: Px) = px.Index = px.Tokens.Length

let private shift (px: Px) : Token =
  assert (px.Index < px.Tokens.Length)
  // trace "shift: %d %A" px.Index px.Tokens.[px.Index]

  let token, _ = px.Tokens.[px.Index]
  px.Index <- px.Index + 1
  token

let inline private bump (px: Px) : unit = shift px |> ignore

let inline private skip (px: Px) : unit = shift px |> ignore

// -----------------------------------------------
// Tokens
// -----------------------------------------------

let private expectPun (token: Token) (px: Px) =
  if look 0 px = token then
    bump px
  else
    error (sprintf "Expected '%A'" token) px

let private eatPun (token: Token) (px: Px) = if look 0 px = token then bump px

// -----------------------------------------------
// Types
// -----------------------------------------------

let private parsePrimaryTy (px: Px) =
  match look 0 px with
  | Token.Ident "int" ->
    bump px
    Ty.Int

  | Token.Ident "string" ->
    bump px
    Ty.String

  | Token.Ident "array" ->
    bump px
    expectPun Token.LeftParen px
    let itemTy = parseTy px
    expectPun Token.RightParen px
    Ty.Array itemTy

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
  | Token.True -> true
  | _ -> false

let private parseExpr (px: Px) : Expr = parseBinary 1 px

let private parsePrimaryExpr (px: Px) : Expr =
  let parseArgs (px: Px) =
    let rec go acc px =
      if atExpr px then
        let itemExpr = parseExpr px
        eatPun Token.Comma px
        go (itemExpr :: acc) px
      else
        List.rev acc

    go [] px

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
    | Token.LeftParen ->
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
    let ty = parseTy px
    let items = parseArgs px
    expectPun Token.RightBracket px
    Expr.Array(items, ty)

  | Token.LeftBrace ->
    bump px
    let ty = parseTy px

    let entries =
      let rec go acc (px: Px) =
        match look 0 px with
        | Token.Ident field ->
          bump px
          expectPun Token.Colon px
          let init = parseExpr px
          go ((field, init) :: acc) px

        | _ -> List.rev acc

      go [] px

    expectPun Token.RightBrace px
    Expr.Record(entries, ty)

  | Token.False ->
    bump px
    Expr.Bool false

  | Token.True ->
    bump px
    Expr.Bool true

  | _ -> error "Expected an expressions" px

let private parsePathExpr (px: Px) =
  let rec go acc (px: Px) =
    match look 0 px with
    | Token.LeftBracket ->
      bump px
      let index = parseExpr px
      expectPun Token.RightBracket px
      go (Expr.Index(acc, index)) px

    | Token.Dot ->
      bump px

      match look 0 px with
      | Token.Ident field ->
        bump px
        go (Expr.Field(acc, field)) px

      | _ -> error "Expected a field" px

    | _ -> acc

  let expr = parsePrimaryExpr px
  go expr px

let private parsePrefix (px: Px) =
  match look 0 px with
  | Token.Bang ->
    bump px

    let arg = parsePathExpr px
    Expr.Call("!", [ arg ])

  | Token.Hyphen ->
    bump px

    let arg = parsePathExpr px
    Expr.Call("-", [ arg ])

  | _ -> parsePathExpr px

let private parseBinary level (px: Px) : Expr =
  let parseArg (px: Px) =
    if level = 5 then
      parsePrefix px
    else
      parseBinary (level + 1) px

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
  | Token.Loop
  | Token.Return
  | Token.While -> true
  | _ -> atExpr px

let private parseStmt (px: Px) : Stmt =
  match look 0 px with
  | Token.Break ->
    bump px
    Stmt.Break

  | Token.Continue ->
    bump px
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

  | Token.Loop ->
    bump px
    let block = parseBlock px
    Stmt.Loop(Stmt.Block block)

  | Token.Return ->
    bump px
    let arg = parseExpr px
    Stmt.Return arg

  | Token.While ->
    bump px
    let cond = parseExpr px
    let body = parseBlock px

    Stmt.If(cond, Stmt.Block body, Stmt.Break)
    |> Stmt.Loop

  | _ ->
    let l = parsePathExpr px

    match look 0 px with
    | Token.Equal ->
      bump px
      let r = parseExpr px
      Stmt.Assign(l, r)

    | _ -> Stmt.Do l

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
  | Token.LeftBrace ->
    let block = parseBlock px
    Decl.Fn("", [], Ty.Name "void", Stmt.Block block)

  | Token.Fn ->
    bump px

    let name =
      match look 0 px with
      | Token.Ident name ->
        bump px
        name

      | _ -> error "Expected a function name" px

    let paramList =
      let rec go acc (px: Px) =
        match look 0 px with
        | Token.Ident name ->
          bump px
          expectPun Token.Colon px
          let ty = parseTy px
          eatPun Token.Comma px
          go ((name, ty) :: acc) px

        | _ -> acc

      go [] px

    eatPun Token.Colon px
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
        | Token.Ident field ->
          expectPun Token.Colon px
          let ty = parseTy px
          go ((field, ty) :: acc) px

        | _ -> acc

      go [] px |> List.rev

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
