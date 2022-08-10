module rec OptLang.Syntax

/// `(y <<< 8) ||| x`
type Pos = uint

// Deriving equality, comparison.
[<RequireQualifiedAccess>]
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
  | Bang
  | BangEqual
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
  | Semi
  | Slash
  | Star

  // keywords:
  | Break
  | Continue
  | Else
  | False
  | Fn
  | If
  | Let
  | Loop
  | Return
  | Then
  | True
  | Type
  | While
  | Void

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Ty =
  | Void
  | Bool
  | Int
  | String
  | Name of name: string
  | Array of Ty

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Expr =
  | Name of string
  | Int of value: int
  | String of string
  | Array of Expr list
  | Record of (string * Expr) list
  | Index of Expr * index: Expr
  | Field of Expr * field: string
  | Call of name: string * Expr list

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Block =
  { Locals: (string * Ty) list
    Stmts: Stmt list }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Stmt =
  | Do of Expr
  | Assign of place: Expr * value: Expr
  | Let of ident: string * Ty option * init: Expr

  // jumps:
  | Break
  | Continue
  | Return of Expr option

  // blocks:
  | Block of Block
  | If of cond: Expr * body: Stmt * alt: Stmt
  | Loop of body: Stmt

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Decl =
  | Block of Block
  | Fn of name: string * paramList: (string * Ty) list * resultTy: Ty * body: Stmt
  | RecordTy of name: string * fields: (string * Ty) list
