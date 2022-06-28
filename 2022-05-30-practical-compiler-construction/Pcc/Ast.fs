module Pcc.Ast

/// 型
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Typ =
  | Int
  | Array of len: int
  | Name of id: string

/// 宣言
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Dec =
  | Var of Typ * ids: string list
  | Type of id: string * Typ
  | Func of result: Typ * id: string * fargs: (Typ * string) list * Block
  | VoidFunc of id: string * fargs: (Typ * string) list * Block

/// ブロック
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Block = { Decs: Dec list; Stmts: Stmt list }

/// 文
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Stmt =
  | Assign of id: string * value: Expr
  | IndexAssign of id: string * index: Expr * value: Expr
  | If of Cond * Stmt
  | IfElse of Cond * Stmt * Stmt
  | While of Cond * Stmt
  | SPrint of Expr
  | IPrint of Expr
  | Scan of id: string
  | New of id: string
  | CallProc of id: string * Expr list
  | Return of Expr
  | Block of Block
  | Nil

/// 式
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Expr =
  | Num of value: int
  | Name of id: string
  | Call of id: string * Expr list
  | Index of id: string * Expr
  | Plus of Expr * Expr
  | Minus of Expr * Expr
  | Times of Expr * Expr
  | Div of Expr * Expr
  | UMinus of Expr
  | Paren of Expr

/// 条件式
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Cond =
  | Eq of Expr * Expr
  | Neq of Expr * Expr
  | Gt of Expr * Expr
  | Lt of Expr * Expr
  | Ge of Expr * Expr
  | Le of Expr * Expr
