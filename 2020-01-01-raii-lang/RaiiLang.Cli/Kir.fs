module rec RaiiLang.Kir

open RaiiLang.Helpers
open RaiiLang.Syntax

type KTy =
  | KIntTy
  | KFunTy
    of (Mode * KTy) list

type KPrim =
  | KEqPrim
  | KAddPrim
  | KAssignPrim
  | KExternFnPrim
    of string

[<Struct>]
type KParam =
  | KParam
    of Mode * name:string

[<Struct>]
type KArg =
  | KArg
    of PassBy * node:KNode

type KNode =
  | KBool
    of bool

  | KInt
    of intText:string

  | KStr
    of StrSegment list

  | KPrim
    of prim:KPrim
      * args:KArg list
      * result:string
      * next:KNode

  | KName
    of name:string

  | KApp
    of cal:string
      * args:KArg list

  | KIf
    of cond:KNode
      * body:KNode
      * alt:KNode

  | KFix
    of name:string
      * paramList:KParam list
      * body:KNode
      * next:KNode

let kPrimFromBin bin =
  match bin with
  | AEqBin ->
    KEqPrim

  | AAddBin ->
    KAddPrim

  | AAssignBin ->
    KAssignPrim

let kPrimToSig prim =
  match prim with
  | KEqPrim ->
    [ByIn; ByIn]

  | KAddPrim ->
    [ByMove; ByMove]

  | KAssignPrim ->
    [ByRef; ByMove]

  | KExternFnPrim _ ->
    failwith "NEVER"

let kPrimToString prim =
  match prim with
  | KEqPrim ->
    "prim_eq"

  | KAddPrim ->
    "prim_add"

  | KAssignPrim ->
    "prim_assign"

  | KExternFnPrim name ->
    sprintf "extern_%s" name
