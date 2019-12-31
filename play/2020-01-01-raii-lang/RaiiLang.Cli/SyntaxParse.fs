module rec RaiiLang.SyntaxParse

open RaiiLang.Helpers
open RaiiLang.Syntax
open RaiiLang.SyntaxParseContext

type P = ParseContext

let inline is actual expected =
  assert (actual = expected)

let tokenIsStmtKeyword token =
  match token with
  | LetToken
  | FnToken ->
    true

  | _ ->
    false

let tokenIsAtomFirst token =
  match token with
  | IntToken
  | IdentToken
  | AssertToken
  | LeftParenToken
  | LeftBraceToken ->
    true

  | _ ->
    false

let tokenIsTermFirst token =
  tokenIsAtomFirst token

let tokenIsParamFirst token =
  match token with
  | IdentToken
  | RefToken ->
    true

  | _ ->
    false

let tokenIsStmtFirst token =
  tokenIsStmtKeyword token
  || tokenIsTermFirst token

let parseLiteralTerm (p: P) =
  p.StartNode()
  p.Eat(IntToken) |> is true
  p.EndNode(LiteralNode)

let parseNameTerm (p: P) =
  p.StartNode()
  (p.Eat(IdentToken) || p.Eat(AssertToken)) |> is true
  p.EndNode(NameNode)

let parseGroupTerm (p: P) =
  p.StartNode()

  p.Eat(LeftParenToken) |> is true

  parseTerm p

  if p.Eat(RightParenToken) |> not then
    p.AddError(ExpectedError "右カッコ")

  p.EndNode(GroupNode)

let parseBlockTerm (p: P) =
  p.StartNode()

  p.Eat(LeftBraceToken) |> is true

  parseSemi p

  if p.Eat(RightBraceToken) |> not then
    p.AddError(ExpectedError "右カッコ")

  p.EndNode(BlockNode)

let parseAtomTerm (p: P) =
  match p.Next with
  | IntToken ->
    parseLiteralTerm p

  | IdentToken
  | AssertToken ->
    parseNameTerm p

  | LeftParenToken ->
    parseGroupTerm p

  | LeftBraceToken ->
    parseBlockTerm p

  | _ ->
    p.Next |> tokenIsAtomFirst |> is false

let parseCallTerm (p: P) =
  parseAtomTerm p

  while p.Next = LeftParenToken do
    p.StartNodeWithPrevious()
    p.StartNode()
    parseGroupTerm p
    p.EndNode(ArgNode)
    p.EndNode(CallNode)

let parseAddTerm (p: P) =
  parseCallTerm p

  while p.Next = PlusToken do
    p.StartNodeWithPrevious()
    parseCallTerm p
    p.EndNode(BinNode)

let parseEqTerm (p: P) =
  parseAddTerm p

  while p.Next = EqualEqualToken do
    p.StartNodeWithPrevious()
    parseAddTerm p
    p.EndNode(BinNode)

let parseAssignTerm (p: P) =
  parseEqTerm p

  if p.Next = EqualToken then
    p.StartNodeWithPrevious()
    parseAssignTerm p
    p.EndNode(BinNode)

let parseTerm (p: P) =
  parseAssignTerm p

let parseParam (p: P) =
  p.StartNode()

  p.Eat(RefToken) |> ignore
  p.Eat(IdentToken) |> ignore

  p.EndNode(ParamNode)

let parseStmt (p: P) =
  match p.Next with
  | LetToken ->
    p.StartNode()
    p.Eat(LetToken) |> is true

    if p.Eat(IdentToken) |> not then
      p.AddError(ExpectedError "識別子")

    if p.Eat(EqualToken) |> not then
      p.AddError(ExpectedError "=")

    if p.Next |> tokenIsTermFirst then
      parseTerm p
    else
      p.AddError(ExpectedError "式")

    p.Eat(SemiToken) |> ignore
    p.EndNode(LetNode)

  | FnToken ->
    p.StartNode()
    p.Eat(FnToken) |> is true

    if p.Eat(IdentToken) |> not then
      p.AddError(ExpectedError "関数名")

    // 引数リスト
    if p.Eat(LeftParenToken) |> not then
      p.AddError(ExpectedError "左カッコ")

    if p.Next |> tokenIsParamFirst then
      parseParam p

    if p.Eat(RightParenToken) |> not then
      p.AddError(ExpectedError "右カッコ")

    // 本体
    if p.Next = LeftBraceToken then
      parseBlockTerm p
    else
      p.AddError(ExpectedError "ブロック")

    p.EndNode(FnNode)

  | _ ->
    p.Next |> tokenIsTermFirst |> is true

    p.StartNode()
    parseTerm p
    p.EndNode(ExprNode)

let parseSemi (p: P) =
  p.StartNode()

  while p.Next |> tokenIsStmtFirst do
    parseStmt p

  p.EndNode(SemiNode)

let parseRoot (p: P) =
  p.StartNode()

  while not p.AtEof do
    if p.Next |> tokenIsStmtFirst then
      parseStmt p
    else
      p.AddError(ExpectedError "文")

      while not (p.AtEof || p.Next |> tokenIsStmtKeyword) do
        p.Bump()

  p.EndNode(SemiNode)

  if p.Eat(EofToken) |> not then
    p.AddError(ExpectedError "anything but EOF")
