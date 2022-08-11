module rec OptLang.Tir

open OptLang.Symbol

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Unary =
  | Not
  | Minus
  | ArrayLen

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Binary =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Equal
  | NotEqual
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual

// derive equality, comparison
[<RequireQualifiedAccess>]
type Ty =
  | Void
  | Bool
  | Int
  | String
  | Record of Symbol
  | Array of Ty

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Callable =
  | Fn of Symbol
  | LogOr
  | LogAnd
  | ArrayPush
  | Assert

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Place =
  | Local of Symbol
  | Index of Place * index: Expr
  | Field of Place * field: Symbol

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Expr =
  | Void
  | Bool of value: bool
  | Int of value: int
  | String of value: string
  | Read of Place
  | Array of itemTy: Ty * Expr list
  | Record of recordTy: Symbol * Expr list
  | Unary of Unary * Expr
  | Binary of Binary * Expr * Expr
  | Call of Callable * Expr list

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Block = { Stmts: Stmt list }

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
  | If of cond: Expr * body: Stmt * alt: Stmt
  | Loop of body: Stmt

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Decl =
  | Block of Block
  | Fn of Symbol * paramList: (Symbol * Ty) list * resultTy: Ty * locals: (Symbol * Ty) list * body: Stmt
  | RecordTy of Symbol * fields: (Symbol * Ty) array
