module rec Pcc.Ast

/// 変数または要素
[<NoEquality; NoComparison>]
type Var =
  | Var of string
  | IndexedVar of string * Expr

/// 宣言
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Dec =
  | Func of name: string * fargs: (Typ * string) list * Typ * Stmt
  | Type of name: string * Typ
  | Var of Typ * name: string

/// 文
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Stmt =
  | Assign of Var * Expr
  | CallProc of name: string * Expr list
  | Block of Dec list * Stmt list
  | If of Expr * Stmt * Stmt option
  | While of Expr * Stmt
  | Nil

/// 式
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Expr =
  | Var of Var
  | Str of text: string
  | Num of value: int
  | Call of name: string * Expr list

/// 型
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Typ =
  | Name of string
  | Array of len: int
  | Int
  | Void
