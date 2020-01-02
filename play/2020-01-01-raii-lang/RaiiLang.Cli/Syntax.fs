module rec RaiiLang.Syntax

open RaiiLang.Helpers

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

  | StrStartToken
  | StrVerbatimToken
  | StrEndToken

  // キーワード類
  | IdentToken
  | AssertToken
  | ExternToken
  | FnToken
  | InToken
  | LetToken
  | MoveToken
  | MutToken
  | RefToken

  // 記号類
  | CommaToken
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
  // 項
  | NameNode
  | IntLiteralNode
  | StrLiteralNode
  | GroupNode
  | BlockNode
  | CallNode
  | BinNode

  // 引数
  | ParamNode
  | ArgNode

  // 文
  | ExprNode
  | LetNode
  | ExternFnNode
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

[<Struct>]
type AName =
  | AName
    of string option * NodeData

[<Struct>]
type AArg =
  | AArg
    of PassBy * ATerm option * NodeData

[<Struct>]
type AParam =
  | AParam
    of Mode * AName option * NodeData

type ATerm =
  | AIntLiteral
    of text:string option * NodeData

  | AStrLiteral
    of StrSegment list * NodeData

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
    of AParam option * AArg option * NodeData

  | AExternFnStmt
    of AName option * AParam list * NodeData

  | AFnStmt
    of AName option * AParam list * ATerm option * NodeData

  | ASemiStmt
    of AStmt list * NodeData

let keywords =
  [
    AssertToken, "assert"
    ExternToken, "extern"
    FnToken, "fn"
    InToken, "in"
    MoveToken, "move"
    MutToken, "mut"
    LetToken, "let"
    RefToken, "ref"
  ]

let punctuations =
  [
    CommaToken, ","
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
