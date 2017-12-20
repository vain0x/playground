[<AutoOpen>]
module Syntax

open System
open Support.Error

type ty =
  | TyTop
  | TyBot
  | TyRecord of (string * ty) list
  | TyArr of ty * ty
  | TyUnit

type term =
  | TmVar    of info * int * int
  | TmAbs    of info * string * ty * term
  | TmApp    of info * term * term
  | TmRecord of info * (string * term) list
  | TmProj   of info * term * string
  | TmUnit   of info

type binding =
  | NameBind 
  | VarBind of ty

type context =
  (string * binding) list

type command =
  | Eval of info * term
  | Bind of info * string * binding
  
exception NoMatchError

(* ---------------------------------------------------------------------- *)
(* Context management *)

let emptycontext = []

let ctxlength ctx = List.length ctx

let addbinding (ctx: context) x bind = (x, bind) :: ctx
  
let addname x ctx = addbinding ctx x NameBind

let index2name fi ctx i =
  try
    List.nth ctx i |> fst
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
        i (List.length ctx)
    error fi msg

let rec name2index fi ctx name =
  ctx
  |> List.tryFindIndex (fst >> (=) name)
  |> Option.getOr' (fun () ->
      error fi ("Identifier " + name + " is unbound"))

let rec isnamebound ctx x =
  ctx |> List.tryFind (fst >> (=) x) |> Option.isSome
 
let rec pickfreshname ctx x =
  if isnamebound ctx x
    then pickfreshname ctx (x + "'")
    else ((x, NameBind) :: ctx, x)
    
let rec getbinding fi ctx i =
  try
    List.nth ctx i |> snd
  with Failure _ ->
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d"
        i (List.length ctx)
    error fi msg

let getTypeFromContext fi (ctx: context) i =
   match getbinding fi ctx i with
   | VarBind (tyT) -> tyT
   | _ ->
      let msg =
        "getTypeFromContext: Wrong kind of binding for variable " 
        + (string <| index2name fi ctx i)
      error fi msg

(* ---------------------------------------------------------------------- *)

// 項構造を写す
// onvar: 変数を写す関数
// c: 環境の深さ
let tmmap onvar c t =
  let rec walk c = function
    | TmVar (fi, x, n) -> onvar fi c x n
    | TmAbs (fi, x, tyT1, t2) -> TmAbs (fi, x, tyT1, walk (c + 1) t2)
    | TmApp (fi, t1, t2) -> TmApp (fi, walk c t1, walk c t2)
    | TmProj (fi, t1, l) -> TmProj (fi, walk c t1, l)
    | TmRecord (fi, fields) ->
        TmRecord (fi, fields |> List.map (fun (li, ti) -> (li, walk c ti)))
    | TmUnit fi -> TmUnit fi
  in
  walk c t

(* Shifting *)

let termShiftAbove d c t =
  let shift_var fi c x n =
    let d' = if x >= c then d else 0
    TmVar (fi, x + d', n + d)
  (c, t) ||> tmmap shift_var

let termShift d t =
  termShiftAbove d 0 t

(* Substitution *)

let termSubst j s t =
  let subst_var fi j x n =
    if x = j
      then termShift j s
      else TmVar (fi, x, n)
  (j, t) ||> tmmap subst_var

let termSubstTop s t = 
  termShift (-1) (termSubst 0 (termShift 1 s) t)

(* Extracting file info *)

let tmInfo = function
  | TmVar (fi, _, _) -> fi
  | TmAbs (fi, _, _, _) -> fi
  | TmApp (fi, _, _) -> fi
  | TmProj (fi, _,_) -> fi
  | TmRecord (fi, _) -> fi
  | TmUnit (fi) -> fi
