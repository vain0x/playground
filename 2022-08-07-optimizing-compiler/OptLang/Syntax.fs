module rec OptLang.Syntax

/// `(y <<< 8) ||| x`
type Pos = uint

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Token =
  | Bad
  | Blank
  | Newlines
  | Comment

  | Int of int
  | String of string
  | Ident of string

  // punctuations:
  | LeftParen
  | RightParen
  | LeftBracket
  | RightBracket
  | LeftBrace
  | RightBrace
  | LeftAngle
  | RightAngle
  | Amp
  | AmpAmp
  | Arrow
  | Colon
  | Comma
  | Dot
  | DotDot
  | Equal
  | EqualEqual
  | Hat
  | Hyphen
  | LeftEqual
  | RightEqual
  | Percent
  | Pipe
  | PipePipe
  | Plus
  | Slash
  | Star

  // keywords:
  | Break
  | Continue
  | Else
  | False
  | Fn
  | If
  | Loop
  | Then
  | True
  | Type

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Ty =
  | Int
  | String
  | Array of Ty

[<NoEquality; NoComparison>]
type Place =
  | Var of string
  | Item of Place * index: Expr
  | Field of Place * field: string

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Expr =
  | Value of Place
  | Int of value: int
  | Bool of bool
  | String of string
  | Array of Expr list * Ty
  | Record of (string * Expr) list * Ty
  | Call of name: string * Expr list

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Block =
  { Locals: (string * Ty) list
    Body: Stmt list }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Stmt =
  | Do of Expr
  | Assign of Place * Expr

  // jumps:
  | Break
  | Continue
  | Return of Expr

  // blocks:
  | Block of Block
  | If of cond: Expr * body: Block * alt: Block
  | While of cond: Expr * body: Block
  | Loop of body: Block

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Decl =
  | Fn of name: string * paramList: (string * Ty) list * resultTy: Ty * Stmt
  | RecordTy of name: string * fields: (string * Ty) list
