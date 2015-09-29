[<AutoOpen>]
module Core

open Support.Error
open Support.Pervasive
open FSharp.Compatibility.OCaml.Format

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
