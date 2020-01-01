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
  | KAssertEqPrim

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

  | KName
    of name:string

  | KPrim
    of prim:KPrim
      * args:KNode list
      * result:string
      * next:KNode

  | KApp
    of cal:string
      * args:KArg list

  | KFix
    of name:string
      * paramList:KParam list
      * body:KNode
      * next:KNode
