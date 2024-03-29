module rec Pcc.Parser

open Pcc.Ast

type private TokenKind = string

// -----------------------------------------------
// Trace
// -----------------------------------------------

// Set true to print trace logs.
[<Literal>]
let private Trace = false

let private trace fmt =
  Printf.kprintf (if Trace then eprintf "%s\n" else ignore) fmt

// -----------------------------------------------
// Context
// -----------------------------------------------

/// Parser context.
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private Px =
  { Tokens: (TokenKind * string) array
    Index: int }

let private atEof (px: Px) = px.Index = px.Tokens.Length

let private look (offset: int) (px: Px) : TokenKind =
  assert (offset >= 0)

  if px.Index + offset < px.Tokens.Length then
    trace "look: %d %A" (px.Index + offset) px.Tokens.[px.Index + offset]
    fst px.Tokens.[px.Index + offset]
  else
    "$"

let private shift (px: Px) : string * Px =
  assert (px.Index < px.Tokens.Length)
  trace "shift: %d %A" px.Index px.Tokens.[px.Index]

  let _, text = px.Tokens.[px.Index]
  let px = { px with Index = px.Index + 1 }
  text, px

let private skip (px: Px) : Px = snd (shift px)

let inline private error msg = failwithf "Parse error: %s" msg

// -----------------------------------------------
// Tokens
// -----------------------------------------------

let private expectPun text px =
  if look 0 px = text then
    skip px
  else
    error ("Expected '" + text + "'")

let private parseNum px =
  match look 0 px with
  | "NUM" ->
    let text, px = shift px
    int text, px

  | _ -> error "Expected NUM" px

let private parseId px =
  match look 0 px with
  | "ID" -> shift px
  | _ -> error "Expected ID" px

// -----------------------------------------------
// Types
// -----------------------------------------------

let private parsePrimaryTyp px =
  match look 0 px with
  | "INT" ->
    let px = skip px

    if look 0 px = "LS" then
      let px = skip px
      let len, px = parseNum px
      let px = expectPun "RS" px
      Typ.Array len, px
    else
      Typ.Int, px

  | "ID" ->
    let name, px = parseId px
    Typ.Name name, px

  | "LP" ->
    let typ, px = px |> skip |> parseTyp
    let px = expectPun "RP" px
    typ, px

  | _ -> error "Expected type" px

let private parseTyp px = parsePrimaryTyp px

// -----------------------------------------------
// Declarations
// -----------------------------------------------

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private DecOrStmt =
  | D of Dec list
  | S of Stmt

let private parseDecOrStmt px =
  match look 0 px with
  | "TYPE" ->
    let px = skip px
    let name, px = parseId px
    let px = expectPun "ASSIGN" px
    let typ, px = parseTyp px
    let px = expectPun "SEMI" px
    DecOrStmt.D [ Dec.Type(name, typ) ], px

  | "VOID" ->
    let px = skip px
    let name, px = parseId px
    let px = expectPun "LP" px
    let fargs, px = parseFargs px
    let px = expectPun "RP" px
    let block, px = parseBlock px
    DecOrStmt.D [ Dec.Func(name, fargs, Typ.Void, block) ], px

  | "IF" ->
    let px = skip px
    let px = expectPun "LP" px
    let cond, px = parseCond px
    let px = expectPun "RP" px

    let thenStmt, px = parseStmt px

    let stmt, px =
      if look 0 px = "ELSE" then
        let elseStmt, px = parseStmt (skip px)
        Stmt.If(cond, thenStmt, Some elseStmt), px
      else
        Stmt.If(cond, thenStmt, None), px

    DecOrStmt.S stmt, px

  | "WHILE" ->
    let px = skip px
    let px = expectPun "LP" px
    let cond, px = parseCond px
    let px = expectPun "RP" px
    let stmt, px = parseStmt px
    DecOrStmt.S(Stmt.While(cond, stmt)), px

  | "SPRINT" ->
    let px = skip px
    let px = expectPun "LP" px
    let arg, px = parseExpr px
    let px = expectPun "RP" px
    let px = expectPun "SEMI" px
    DecOrStmt.S(Stmt.CallProc("sprint", [ arg ])), px

  | "IPRINT" ->
    let px = skip px
    let px = expectPun "LP" px
    let arg, px = parseExpr px
    let px = expectPun "RP" px
    let px = expectPun "SEMI" px
    DecOrStmt.S(Stmt.CallProc("iprint", [ arg ])), px

  | "SCAN" ->
    let px = skip px
    let px = expectPun "LP" px
    let name, px = parseId px
    let px = expectPun "RP" px
    let px = expectPun "SEMI" px
    DecOrStmt.S(Stmt.CallProc("scan", [ Expr.Var(Var name) ])), px

  | "NEW" ->
    let px = skip px
    let px = expectPun "LP" px
    let name, px = parseId px
    let px = expectPun "RP" px
    let px = expectPun "SEMI" px
    DecOrStmt.S(Stmt.CallProc("new", [ Expr.Var(Var name) ])), px

  | "RETURN" ->
    let px = skip px
    let arg, px = parseExpr px
    let px = expectPun "SEMI" px
    DecOrStmt.S(Stmt.CallProc("return", [ arg ])), px

  | "LB" ->
    let block, px = parseBlock px
    DecOrStmt.S block, px

  | "SEMI" -> DecOrStmt.S Stmt.Nil, skip px

  | "INT"
  | "LP" -> parseVarOrFuncDec px

  | "ID" ->
    match look 1 px with
    | "ASSIGN" ->
      let name, px = shift px
      let px = skip px
      let value, px = parseExpr px
      let px = expectPun "SEMI" px
      DecOrStmt.S(Stmt.Assign(Var name, value)), px

    | "LP" ->
      let name, px = shift px
      let px = skip px
      let args, px = parseAargs px
      let px = expectPun "RP" px
      let px = expectPun "SEMI" px
      DecOrStmt.S(Stmt.CallProc(name, args)), px

    | "LS" ->
      let name, px = shift px
      let px = skip px
      let index, px = parseExpr px
      let px = expectPun "RS" px
      let px = expectPun "ASSIGN" px
      let value, px = parseExpr px
      let px = expectPun "SEMI" px
      DecOrStmt.S(Stmt.Assign(IndexedVar(name, index), value)), px

    | _ -> parseVarOrFuncDec px

  | _ -> error "Expected a declaration or a statement."

let private parseVarOrFuncDec px =
  let typ, px = parseTyp px

  match look 1 px with
  | "LP" ->
    let name, px = parseId px
    let px = skip px
    let fargs, px = parseFargs px
    let px = expectPun "RP" px
    let block, px = parseBlock px
    DecOrStmt.D [ Dec.Func(name, fargs, typ, block) ], px

  | _ ->
    let names, px = parseIds px
    let px = expectPun "SEMI" px
    DecOrStmt.D(names |> List.map (fun name -> Dec.Var(typ, name))), px

let private parseIds px =
  let rec go acc px =
    match look 0 px with
    | "ID" ->
      let name, px = shift px
      let acc = name :: acc

      match look 0 px with
      | "COMMA" -> go acc (skip px)
      | _ -> List.rev acc, px

    | _ -> List.rev acc, px

  go [] px

let private parseFargs px =
  let rec go acc px =
    match look 0 px with
    | "RP"
    | "RS"
    | "RB"
    | "SEMI"
    | "$" -> List.rev acc, px

    | _ ->
      let typ, px = parseTyp px
      let name, px = parseId px
      let acc = (typ, name) :: acc

      match look 0 px with
      | "COMMA" -> go acc (skip px)
      | _ -> List.rev acc, px

  go [] px

let private parseAargs px =
  let rec go acc px =
    match look 0 px with
    | "RP"
    | "RS"
    | "RB"
    | "SEMI"
    | "$" -> List.rev acc, px

    | _ ->
      let arg, px = parseExpr px
      let acc = arg :: acc

      match look 0 px with
      | "COMMA" -> go acc (skip px)
      | _ -> List.rev acc, px

  go [] px

let private parseStmt px =
  let it, px = parseDecOrStmt px

  match it with
  | DecOrStmt.D _ -> error "Expected a statement."
  | DecOrStmt.S stmt -> stmt, px

let private parseBlock px =
  let rec go decs stmts px =
    match look 0 px with
    | "RB"
    | "$" -> List.rev decs, List.rev stmts, px

    | _ ->
      let result, px = parseDecOrStmt px

      match result with
      | DecOrStmt.D dec ->
        if List.isEmpty stmts then
          go (dec :: decs) stmts px
        else
          error "Declaration can't appear after any statements."

      | DecOrStmt.S stmt -> go decs (stmt :: stmts) px

  let px = expectPun "LB" px
  let decs, stmts, px = go [] [] px

  if List.isEmpty stmts then
    error "Expected at least one statement."

  let px = expectPun "RB" px
  Stmt.Block(List.collect id decs, stmts), px

// -----------------------------------------------
// Expressions
// -----------------------------------------------

let private parsePrimaryExpr px =
  match look 0 px with
  | "NUM" ->
    let value, px = parseNum px
    Expr.Num value, px

  | "STR" ->
    let text, px = shift px

    assert
      (text.Length >= 2
       && text.[0] = '"'
       && text.[text.Length - 1] = '"')

    let value = text.[1 .. text.Length - 2]

    Expr.Str value, px

  | "ID" ->
    let name, px = shift px

    match look 0 px with
    | "LP" ->
      let px = skip px
      let args, px = parseAargs px
      let px = expectPun "RP" px
      Expr.Call(name, args), px

    | "LS" ->
      let px = skip px
      let index, px = parseExpr px
      let px = expectPun "RS" px
      Expr.Var(IndexedVar(name, index)), px

    | _ -> Expr.Var(Var name), px

  | "LP" ->
    let px = skip px
    let expr, px = parseExpr px
    let px = expectPun "RP" px
    expr, px

  | _ -> error "Expected an expression"

let private parsePrefixExpr px =
  match look 0 px with
  | "MINUS" ->
    let px = skip px
    let arg, px = parsePrimaryExpr px
    Expr.Call("!", [ arg ]), px

  | _ -> parsePrimaryExpr px

let private parseTimesExpr px =
  let rec go lhs px =
    match (match look 0 px with
           | "TIMES" -> Some "*"
           | "DIV" -> Some "/"
           | _ -> None)
      with
    | Some name ->
      let px = skip px
      let rhs, px = parsePrefixExpr px
      go (Expr.Call(name, [ lhs; rhs ])) px

    | None -> lhs, px

  let lhs, px = parsePrefixExpr px
  go lhs px

let private parsePlusExpr px =
  let rec go lhs px =
    match (match look 0 px with
           | "PLUS" -> Some "+"
           | "MINUS" -> Some "-"
           | _ -> None)
      with
    | Some name ->
      let px = skip px
      let rhs, px = parseTimesExpr px
      go (Expr.Call(name, [ lhs; rhs ])) px

    | None -> lhs, px

  let lhs, px = parseTimesExpr px
  go lhs px

let private parseExpr px = parsePlusExpr px

// -----------------------------------------------
// Cond
// -----------------------------------------------

let private parseCond px =
  let lhs, px = parseExpr px

  match (match look 0 px with
         | "EQ" -> Some "=="
         | "NEQ" -> Some "!="
         | "GT" -> Some ">"
         | "LT" -> Some "<"
         | "GE" -> Some ">="
         | "LE" -> Some "<="
         | _ -> None)
    with
  | Some name ->
    let px = skip px
    let rhs, px = parseExpr px
    Expr.Call(name, [ lhs; rhs ]), px

  | None -> error "Expected a comparison operator."

// -----------------------------------------------
// Interface
// -----------------------------------------------

let parseTokens tokens =
  let px: Px = { Tokens = tokens; Index = 0 }
  let prog, px = parseStmt px

  if atEof px |> not then
    error "Expected the end of input."

  prog
