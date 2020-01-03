module rec RaiiLang.SyntaxParse

open RaiiLang.Helpers
open RaiiLang.Syntax
open RaiiLang.SyntaxParseContext

type P = ParseContext

let tokenIsStmtKeyword token =
  match token with
  | ExternToken
  | LetToken
  | FnToken ->
    true

  | _ ->
    false

let tokenIsAtomFirst token =
  match token with
  | FalseToken
  | TrueToken
  | IntToken
  | StrStartToken
  | IdentToken
  | AssertToken
  | LeftParenToken
  | LeftBraceToken ->
    true

  | _ ->
    false

let tokenIsTermFirst token =
  tokenIsAtomFirst token

let tokenIsArgFirst token =
  match token with
  | InToken
  | MoveToken
  | RefToken ->
    true

  | _ ->
    tokenIsTermFirst token

let tokenIsParamFirst token =
  match token with
  | InToken
  | MutToken
  | RefToken ->
    true

  | _ ->
    tokenIsTermFirst token

let tokenIsStmtFirst token =
  tokenIsStmtKeyword token
  || tokenIsTermFirst token

let parseBoolLiteralTerm (p: P) =
  assert (p.Next = FalseToken || p.Next = TrueToken)

  p.StartNode()
  (p.Eat(FalseToken) || p.Eat(TrueToken)) |> is true
  p.EndNode(BoolLiteralNode)

let parseIntLiteralTerm (p: P) =
  assert (p.Next = IntToken)

  p.StartNode()
  p.Bump()
  p.EndNode(IntLiteralNode)

let parseStrLiteralTerm (p: P) =
  assert (p.Next = StrStartToken)

  p.StartNode()
  p.Bump()

  while p.Eat(StrVerbatimToken) do
    ()

  p.Eat(StrEndToken) |> is true

  p.EndNode(StrLiteralNode)

let parseNameTerm (p: P) =
  p.StartNode()
  (p.Eat(IdentToken) || p.Eat(FalseToken) || p.Eat(TrueToken) || p.Eat(AssertToken)) |> is true
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
  | FalseToken
  | TrueToken ->
    parseBoolLiteralTerm p

  | IntToken ->
    parseIntLiteralTerm p

  | StrStartToken ->
    parseStrLiteralTerm p

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
    p.Eat(LeftParenToken) |> is true

    while p.Next |> tokenIsParamFirst
      || p.Next = CommaToken do
      parseArg p
      p.Eat(CommaToken) |> ignore

    if p.Eat(RightParenToken) |> not then
      p.AddError(ExpectedError "右カッコ")

    p.EndNode(CallNode)

let parseAddTerm (p: P) =
  parseCallTerm p

  while p.Next = PlusToken do
    p.StartNodeWithPrevious()
    p.Eat(PlusToken) |> is true
    parseCallTerm p
    p.EndNode(BinNode)

let parseEqTerm (p: P) =
  parseAddTerm p

  while p.Next = EqualEqualToken do
    p.StartNodeWithPrevious()
    p.Eat(EqualEqualToken) |> is true
    parseAddTerm p
    p.EndNode(BinNode)

let parseAssignTerm (p: P) =
  parseEqTerm p

  if p.Next = EqualToken then
    p.StartNodeWithPrevious()
    p.Eat(EqualToken) |> is true
    parseAssignTerm p
    p.EndNode(BinNode)

let parseTerm (p: P) =
  parseAssignTerm p

let parseArg (p: P) =
  p.StartNode()

  (p.Eat(InToken) || p.Eat(MoveToken) || p.Eat(RefToken)) |> ignore
  parseTerm p

  p.EndNode(ArgNode)

let parseParam (p: P) =
  p.StartNode()

  (p.Eat(InToken) || p.Eat(MutToken) || p.Eat(RefToken)) |> ignore
  parseCallTerm p

  p.EndNode(ParamNode)

// `fn name(param*)`
let parseFnHead (p: P) =
  if p.Eat(FnToken) |> not then
    p.AddError(ExpectedError "fn")

  if p.Next = IdentToken then
    parseNameTerm p
  else
    p.AddError(ExpectedError "関数名")

  // 引数リスト
  if p.Eat(LeftParenToken) |> not then
    p.AddError(ExpectedError "左カッコ")

  while p.Next |> tokenIsParamFirst
    || p.Next = CommentToken do
    parseParam p
    p.Eat(CommaToken) |> ignore

  if p.Eat(RightParenToken) |> not then
    p.AddError(ExpectedError "右カッコ")

let parseStmt (p: P) =
  match p.Next with
  | LetToken ->
    p.StartNode()
    p.Eat(LetToken) |> is true

    if p.Next |> tokenIsParamFirst then
      parseParam p
    else
      p.AddError(ExpectedError "変数")

    if p.Eat(EqualToken) |> not then
      p.AddError(ExpectedError "=")

    if p.Next |> tokenIsArgFirst then
      parseArg p
    else
      p.AddError(ExpectedError "式")

    p.Eat(SemiToken) |> ignore
    p.EndNode(LetNode)

  | ExternToken ->
    p.StartNode()

    p.Eat(ExternToken) |> is true
    parseFnHead p
    p.Eat(SemiToken) |> ignore

    p.EndNode(ExternFnNode)

  | FnToken ->
    p.StartNode()
    parseFnHead p

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
    p.Eat(SemiToken) |> ignore
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

let parse (sourceCode: string) =
  let tokens = SyntaxTokenize.tokenize sourceCode
  let p = ParseContext(tokens)
  parseRoot p
  p.Finish()

let nodeToSnapshot (node: NodeData) =
  let w = System.Text.StringBuilder()

  let writeIndent depth =
    for _ in 0..(depth - 1) do
      w.Append("  ") |> ignore

  let writeToken depth (token: TokenData) =
    writeIndent depth
    w.AppendFormat("T({0}) `{1}`\n", token.Token, token.Text) |> ignore

  let rec writeNode depth (node: NodeData) =
    writeIndent depth
    w.AppendFormat("N({0}) [\n", node.Node) |> ignore

    for child in node.Children do
      match child with
      | TokenElement token ->
        writeToken (depth + 1) token

      | NodeElement child ->
        writeNode (depth + 1) child

      | ErrorElement error ->
        writeIndent (depth + 1)
        w.AppendFormat("E({0})\n", error) |> ignore

    writeIndent depth
    w.Append("]\n") |> ignore

  writeNode 0 node
  w.ToString()
