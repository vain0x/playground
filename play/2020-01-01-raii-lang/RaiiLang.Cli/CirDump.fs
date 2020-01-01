module rec RaiiLang.CirDump

open RaiiLang.Cir
open RaiiLang.Helpers

type CBindingPower =
  | CMinBp
  | CAssignBp
  | CEqBp
  | CAddBp
  | CPrefixBp
  | CSuffixBp
  | CAtomBp

let cTyToFunTy ty =
  match ty with
  | CFunTy _ ->
    Some ty

  | CPtrTy innerTy ->
    cTyToFunTy innerTy

  | _ ->
    None

let cBinToBp bin =
  match bin with
  | CEqBin ->
    CEqBp

  | CAddBin ->
    CAddBp

  | CAssignBin ->
    CAssignBp

let cdTy ty acc =
  match ty with
  | CVoidTy ->
    acc |> cons "void"

  | CIntTy ->
    acc |> cons "int"

  | CPtrTy itemTy ->
    acc |> cdTy itemTy |> cons "*"

  | CFunTy _ ->
    failwithf "NEVER: %A" ty

let cdTyList tys acc =
  tys |> List.fold (fun (sep, acc) ty ->
    let acc =
      acc
      |> cons sep
      |> cdTy ty
    ", ", acc
  ) ("", acc)
  |> snd

let cdVarDecl name ty acc =
  match ty |> cTyToFunTy with
  | Some (CFunTy (paramTys, resultTy)) ->
    acc
    |> cdTy resultTy
    |> cons " (*"
    |> cons name
    |> cons ")("
    |> cdTyList paramTys
    |> cons ")"

  | _ ->
    acc
    |> cdTy ty
    |> cons " "
    |> cons name

let cdUni uni acc =
  match uni with
  | CDerefUni ->
    acc |> cons "*"

  | CRefUni ->
    acc |> cons "&"

let cdBin bin acc =
  match bin with
  | CEqBin ->
    acc |> cons "=="

  | CAddBin ->
    acc |> cons "+"

  | CAssignBin ->
    acc |> cons "="

let cdParamList paramList acc =
  paramList |> List.fold (fun (sep, acc) (CParam (name, ty)) ->
    let acc =
      acc
      |> cons sep
      |> cdVarDecl name ty
    ", ", acc
  ) ("", acc)
  |> snd

let cdTerm term (superBp: CBindingPower) acc =
  let paren text (bp: CBindingPower) acc =
    if superBp <= bp then
      acc
    else
      acc |> cons text

  match term with
  | CInt text ->
    acc |> cons text

  | CName name ->
    acc |> cons name

  | CUni (uni, first) ->
    acc
    |> paren "(" CPrefixBp
    |> cdUni uni
    |> cdTerm first CPrefixBp
    |> paren ")" CPrefixBp

  | CCall (cal, args) ->
    acc
    |> paren "(" CSuffixBp
    |> cdTerm cal CAtomBp
    |> cons "("
    |> cdTermList args ", "
    |> cons ")"
    |> paren ")" CSuffixBp

  | CBin (bin, first, second) ->
    let bp = cBinToBp bin

    acc
    |> paren "(" bp
    |> cdTerm first bp
    |> cons " "
    |> cdBin bin
    |> cons " "
    |> cdTerm second bp
    |> paren ")" bp

let cdTermList terms sep acc =
  terms
  |> List.fold (fun (localSep, acc) term ->
    let acc =
      acc
      |> cons localSep
      |> cdTerm term CMinBp
    sep, acc
  ) ("", acc)
  |> snd

let cdStmt stmt acc =
  match stmt with
  | CTermStmt term ->
    acc
    |> cdTerm term CMinBp
    |> cons ";"

  | CLocalStmt (name, ty, body) ->
    acc
    |> cdVarDecl name ty
    |> cons " = "
    |> cdTerm body CMinBp
    |> cons ";"

  | CReturn None ->
    acc |> cons "return;"

  | CReturn (Some body) ->
    acc
    |> cons "return "
    |> cdTerm body CMinBp
    |> cons ";"

let cdStmtList stmts indent acc =
  stmts |> List.fold (fun (sep, acc) stmt ->
    let acc =
      acc
      |> cons sep
      |> cdStmt stmt

    indent, acc
  ) ("", acc)
  |> snd

let cdDecl decl indent acc =
  match decl with
  | CFnDecl (name, paramList, resultTy, body) ->
    let deepIndent = indent + "    "

    let acc =
      acc
      |> cdVarDecl name resultTy
      |> cons "("
      |> cdParamList paramList
      |> cons ") {"

    match body with
    | [] ->
      acc |> cons "}"

    | _ ->
      acc
      |> cons deepIndent
      |> cdStmtList body deepIndent
      |> cons indent
      |> cons "}"

let cdDeclList decls indent acc =
  let deepIndent = eol + indent

  decls |> List.fold (fun (sep, acc) decl ->
    let acc =
      acc
      |> cons sep
      |> cdDecl decl indent

    deepIndent, acc
  ) ("", acc)
  |> snd

let cirDump decls =
  []
  |> cdDeclList decls eol
  |> List.rev
  |> String.concat ""
