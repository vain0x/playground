[<AutoOpen>]
module Core

open Support.Error
open Support.Pervasive
open FSharp.Compatibility.OCaml.Format

(* ------------------------   SUBTYPING  ------------------------ *)

let rec subtype tyS tyT =
  let walk = function
    | (_, TyTop) -> true
    | (TyBot, _) -> true
    | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
           subtype tyT1 tyS1
        && subtype tyS2 tyT2
    | (TyRecord (fS), TyRecord (fT)) ->
        let pred (li, tyTi) =
          List.tryAssoc li fS
          |> Option.map (fun tySj -> subtype tySj tyTi)
          |> (=) (Some true)
        fT |> List.forall pred
    | _ -> false

  (=) tyS tyT || walk (tyS, tyT)

(* ------------------------   TYPING  ------------------------ *)

let rec typeof (ctx: context) = function
  | TmUnit _ ->
      TyUnit
  | TmRecord (fi, fields) ->
      let fieldtys = 
        List.map (fun (li, ti) -> (li, typeof ctx ti)) fields
      TyRecord (fieldtys)
  | TmVar (fi, i, _) -> getTypeFromContext fi ctx i
  | TmAbs (fi, x, tyT1, t2) ->
      let ctx' = addbinding ctx x (VarBind tyT1) in
      let tyT2 = typeof ctx' t2 in
      TyArr(tyT1, tyT2)
  | TmApp (fi, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      match tyT1 with
      | TyArr (tyT11, tyT12) ->
          if subtype tyT2 tyT11 then tyT12
          else error fi "parameter type mismatch" 
      | TyBot -> TyBot
      | _ -> error fi "arrow type expected"
  | TmProj (fi, t1, l) ->
      match (typeof ctx t1) with
      | TyRecord (fieldtys) ->
          List.tryAssoc l fieldtys
          |> Option.getOr' (fun () -> error fi ("label " + l + " not found"))
      | TyBot -> TyBot
      | _ -> error fi "Expected record type"
      
(* ------------------------   EVALUATION  ------------------------ *)

let rec isval ctx = function
  | TmAbs (_, _, _, _) -> true
  | TmRecord (_, fields) -> List.forall (fun (l, ti) -> isval ctx ti) fields
  | _ -> false

exception NoRuleApplies

let rec eval1 ctx = function
  | TmApp(fi,TmAbs(_,x,tyT11,t12),v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp(fi,v1,t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)
  | TmRecord(fi,fields) ->
      let rec evalafield l = match l with 
        [] -> raise NoRuleApplies
      | (l,vi)::rest when isval ctx vi -> 
          let rest' = evalafield rest in
          (l,vi)::rest'
      | (l,ti)::rest -> 
          let ti' = eval1 ctx ti in
          (l, ti')::rest
      in let fields' = evalafield fields in
      TmRecord(fi, fields')
  | TmProj(fi, (TmRecord(_, fields) as v1), l) when isval ctx v1 ->
      List.tryAssoc l fields
      |> Option.getOr' (fun () -> raise NoRuleApplies)
  | TmProj(fi, t1, l) ->
      let t1' = eval1 ctx t1 in
      TmProj(fi, t1', l)
  | _ -> 
      raise NoRuleApplies

let rec eval ctx t =
  try
    let t' = eval1 ctx t
    eval ctx t'
  with NoRuleApplies -> t

(* ---------------------------------------------------------------------- *)
(* Printing *)

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let pr_break() = print_break 0 0

let small = function
  | TmVar (_, _, _) -> true
  | _ -> false

let rec printty_Type outer tyT =
  printty_ArrowType outer tyT

and printty_ArrowType outer = function
  | TyArr(tyT1,tyT2) ->
      obox0(); 
      printty_AType false tyT1;
      if outer then pr " ";
      pr "->";
      if outer then print_space() else pr_break();
      printty_ArrowType outer tyT2;
      cbox()
  | tyT -> printty_AType outer tyT

and printty_AType outer = function
  | TyRecord (fields) ->
      let pf i (li, tyTi) =
          if li <> (string i) then (pr li; pr ":")
          printty_Type false tyTi
      let rec p i = function
          | [] -> ()
          | [f] -> pf i f
          | f::rest ->
              pf i f; pr","; if outer then print_space() else pr_break()
              p (i+1) rest
      pr "{"; open_hovbox 0; p 1 fields; pr "}"; cbox()
  | TyTop -> pr "Top"
  | TyBot -> pr "Bot"
  | TyUnit -> pr "Unit"
  | tyT -> pr "("; printty_Type outer tyT; pr ")"

let printty tyT = printty_Type true tyT 

let rec printtm_Term outer ctx = function
  | TmAbs(fi,x,tyT1,t2) ->
      let (ctx', x') = pickfreshname ctx x
      obox(); pr "lambda ";
      pr x'; pr ":"; printty_Type false tyT1; pr ".";
      if small t2 && not outer
        then pr_break()
        else print_space()
      printtm_Term outer ctx' t2;
      cbox()
  | t -> printtm_AppTerm outer ctx t

and printtm_AppTerm outer ctx = function
  | TmApp(fi, t1, t2) ->
      obox0();
      printtm_AppTerm false ctx t1;
      print_space();
      printtm_ATerm false ctx t2;
      cbox()
  | t -> printtm_PathTerm outer ctx t

and printtm_PathTerm outer ctx = function
  | TmProj(_, t1, l) ->
      printtm_ATerm false ctx t1; pr "."; pr l
  | t -> printtm_ATerm outer ctx t

and printtm_ATerm outer ctx = function
  | TmUnit (fi) ->
      pr "unit"
  | TmVar (fi, x, n) ->
      if ctxlength ctx = n then
        pr (index2name fi ctx x)
      else
        pr ("[bad index: " + (string x) + "/" + (string n)
            + " in {"
            + (List.fold (fun s (x, _) -> s + " " + x) "" ctx)
            + " }]")
  | TmRecord(fi, fields) ->
       let pf i (li,ti) =
         if (li <> ((string i))) then (pr li; pr "=")
         printtm_Term false ctx ti
       let rec p i = function
         | [] -> ()
         | [f] -> pf i f
         | f::rest ->
             pf i f; pr","; if outer then print_space() else pr_break(); 
             p (i+1) rest
       pr "{"; open_hovbox 0; p 1 fields; pr "}"; cbox()
  | t -> pr "("; printtm_Term outer ctx t; pr ")"

let printtm ctx t = printtm_Term true ctx t 

let prbinding ctx = function
  | NameBind -> ()
  | VarBind(tyT) -> pr ": "; printty tyT 
