module rec OptLang.Mir

open OptLang.Symbol

[<RequireQualifiedAccess; ReferenceEquality>]
type MLocalDef = { Name: string; Ty: MTy }

[<RequireQualifiedAccess; ReferenceEquality>]
type MFnDef =
  { Name: string
    Params: (Symbol * MTy) array
    ResultTy: MTy
    Locals: Map<Symbol, MLocalDef>
    Blocks: MBlockDef array }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type MBlockDef =
  { Stmts: MStmt array
    Terminator: MTerminator }

[<RequireQualifiedAccess; ReferenceEquality>]
type MFieldDef = { Name: string; Ty: MTy }

[<RequireQualifiedAccess; ReferenceEquality>]
type RecordDef =
  { Name: string
    Fields: MFieldDef array }

[<RequireQualifiedAccess; ReferenceEquality>]
type MArrayDef = { ItemTy: MTy }

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
type MPart =
  | Index of MRval * array: Symbol
  | Field of index: int * record: Symbol

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type MPlace = { Local: Symbol; Path: MPart array }

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
  | Call of MPlace * MCallable * MRval array

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type MTerminator =
  | Unreachable
  | Goto of Symbol
  | Return
  | If of cond: MRval * body: Symbol * alt: Symbol

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type MBodyDef =
  { Locals: Map<Symbol, MLocalDef>
    Blocks: MBlockDef array }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type MProgram =
  { Bodies: MBodyDef array
    Fns: Map<Symbol, MFnDef>
    Records: Map<Symbol, RecordDef>
    Arrays: MArrayDef array }
