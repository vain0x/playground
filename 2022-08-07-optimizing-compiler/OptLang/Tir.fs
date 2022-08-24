module rec OptLang.Tir

open OptLang.Symbol

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TUnary =
  | Not
  | Minus
  | ArrayLen

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TBinary =
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
type TTy =
  | Void
  | Bool
  | Int
  | String
  | Record of Symbol
  | Array of TTy

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TCallable =
  | Fn of Symbol
  | LogOr
  | LogAnd
  | ArrayPush
  | Assert

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TPlace =
  | Local of Symbol
  | Index of TPlace * index: TExpr
  | Field of TPlace * field: Symbol

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TExpr =
  | Void
  | Bool of value: bool
  | Int of value: int
  | String of value: string
  | Read of TPlace
  | Array of itemTy: TTy * TExpr list
  | Record of recordTy: Symbol * TExpr list
  | Unary of TUnary * TExpr
  | Binary of TBinary * TExpr * TExpr
  | Call of TCallable * TExpr list

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TBlock = { Stmts: TStmt list }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TStmt =
  | Do of TExpr
  | Assign of TPlace * TExpr

  // jumps:
  | Break
  | Continue
  | Return of TExpr

  // blocks:
  | Block of TBlock
  | If of cond: TExpr * body: TStmt * alt: TStmt
  | Loop of body: TStmt

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TDecl =
  | Block of locals: (Symbol * TTy) list * TBlock
  | Fn of Symbol * paramList: (Symbol * TTy) list * resultTy: TTy * locals: (Symbol * TTy) list * body: TStmt
  | RecordTy of Symbol * fields: (Symbol * TTy) array
