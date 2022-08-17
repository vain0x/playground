module rec OptLang.Mir

open OptLang.Symbol

type Label = Symbol

[<RequireQualifiedAccess; ReferenceEquality>]
type LocalDef = { Name: string; Ty: MTy }

[<RequireQualifiedAccess; ReferenceEquality>]
type FnDef =
  { Name: string
    Params: (Symbol * MTy) array
    ResultTy: MTy
    Locals: Map<Symbol, LocalDef>
    Blocks: BlockDef array }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type BlockDef =
  { Stmts: MStmt array
    Terminator: MTerminator }

[<RequireQualifiedAccess; ReferenceEquality>]
type FieldDef = { Name: string; Ty: MTy }

[<RequireQualifiedAccess; ReferenceEquality>]
type RecordDef =
  { Name: string
    Fields: FieldDef array }

[<RequireQualifiedAccess; ReferenceEquality>]
type ArrayDef = { ItemTy: MTy }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type MUnary =
  | Not
  | Minus
  | ArrayLen

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type MBinary =
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

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type MCallable =
  | Fn of Symbol
  | ArrayPush
  | Assert

// derive equality, comparison
[<RequireQualifiedAccess>]
type MTy =
  | Void
  | Bool
  | Int
  | String
  | Record of Symbol
  | Array of Symbol

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Part =
  | Index of MRval * array: Symbol
  | Field of index: int * record: Symbol

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type MPlace = { Local: Symbol; Path: Part array }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type MRval =
  | Void
  | Bool of value: bool
  | Int of value: int
  | String of value: string
  | Read of MPlace
  | Unary of MUnary * MRval
  | Binary of MBinary * MRval * MRval
  | Record of MRval array * record: Symbol
  | Array of MRval array * array: Symbol

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type MStmt =
  | Assign of MPlace * MRval
  | Call of MCallable * MRval array

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type MTerminator =
  | Unreachable
  | Goto of Label
  | Return
  | If of cond: MRval * body: Label * alt: Label

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type MBody =
  { ParamCount: int
    Blocks: BlockDef array }
