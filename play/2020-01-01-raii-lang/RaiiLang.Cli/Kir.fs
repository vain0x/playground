module rec RaiiLang.Kir

open RaiiLang.Helpers
open RaiiLang.Syntax

type KTy =
  | KIntTy
  | KFunTy
    of (CallBy * KTy) list

type KPrim =
  | KEqPrim
  | KAddPrim
  | KAssignPrim
  | KExternFnPrim
    of string

[<Struct>]
type KParam =
  | KParam
    of callBy:CallBy * name:string

[<Struct>]
type KArg =
  | KArg
    of callBy:CallBy * node:KNode

type KNode =
  | KInt
    of intText:string

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

  | KFix
    of name:string
      * paramList:KParam list
      * body:KNode
      * next:KNode

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
