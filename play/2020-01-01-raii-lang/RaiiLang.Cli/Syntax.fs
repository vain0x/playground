module rec RaiiLang.Syntax

type TextLength = int

[<Struct>]
type SyntaxError =
  | ExpectedError
    of string

  | UnexpectedCharsError

type Token =
  | EofToken
  | EolToken
  | SpaceToken
  | CommentToken
  | OtherToken

  | IntToken

  // キーワード類
  | IdentToken
  | AssertToken
  | FnToken
  | LetToken
  | RefToken

  // 記号類
  | EqualEqualToken
  | EqualToken
  | LeftBraceToken
  | LeftParenToken
  | PlusToken
  | RightBraceToken
  | RightParenToken
  | SemiToken

[<Struct>]
type TokenData =
  {
    Token: Token
    Text: string
  }

type TriviaData =
  TokenData

[<Struct>]
type TokenFat =
  {
    Token: TokenData
    Leading: ResizeArray<TriviaData>
    Trailing: ResizeArray<TriviaData>
  }

type Node =
  | NameNode
  | LiteralNode
  | GroupNode
  | BlockNode
  | ArgNode
  | CallNode
  | BinNode
  | ExprNode
  | LetNode
  | ParamNode
  | FnNode
  | SemiNode

[<Struct>]
type NodeData =
  {
    Node: Node
    Children: ResizeArray<Element>
  }

and [<Struct>] Element =
  | TokenElement
    of token:TokenData

  | NodeElement
    of node:NodeData

  | ErrorElement
    of error:SyntaxError

type ABin =
  | AEqBin
  | AAddBin
  | AAssignBin

type AName =
  | AName
    of string option * NodeData

type AArg =
  | ARefArg
    of AName option * NodeData

type ATerm =
  | AIntLiteral
    of NodeData

  | ANameTerm
    of AName

  | AGroupTerm
    of ATerm option * NodeData

  | ABlockTerm
    of AStmt list * NodeData

  | ACallTerm
    of ATerm option * AArg list * NodeData

  | ABinTerm
    of ABin option * ATerm option * ATerm option * NodeData

type AStmt =
  | ATermStmt
    of ATerm option * NodeData

  | ALetStmt
    of ATerm option * ATerm option * NodeData

  | AFnStmt
    of AName option * AArg list * AStmt option * NodeData

  | ASemiStmt
    of AStmt list * NodeData

let keywords =
  [
    AssertToken, "assert"
    FnToken, "fn"
    LetToken, "let"
    RefToken, "ref"
  ]

let punctuations =
  [
    EqualEqualToken, "=="
    EqualToken, "="
    LeftBraceToken, "{"
    LeftParenToken, "("
    PlusToken, "+"
    RightBraceToken, "}"
    RightParenToken, ")"
    SemiToken, ";"
  ]

let tokenIsTrivia token =
  match token with
  | EolToken
  | SpaceToken
  | CommentToken
  | OtherToken ->
    true

  | _ ->
    false

let tokenIsLeadingTrivia token =
  tokenIsTrivia token

let tokenIsTrailingTrivia token =
  tokenIsTrivia token && token <> EolToken

let nodeAddTokenFat (token: TokenFat) (node: NodeData) =
  for trivia in token.Leading do
    node.Children.Add(TokenElement trivia)

  node.Children.Add(TokenElement token.Token)

  for trivia in token.Trailing do
    node.Children.Add(TokenElement trivia)
