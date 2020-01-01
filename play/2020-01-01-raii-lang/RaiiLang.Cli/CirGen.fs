module rec RaiiLang.CirGen

open RaiiLang.Cir
open RaiiLang.Helpers
open RaiiLang.Kir
open RaiiLang.KirGen
open RaiiLang.KirInfer

type CirGenContext =
  {
    TyMap: HashMap<string, KTy>
    Stmts: ResizeArray<CStmt>
    Decls: ResizeArray<CDecl>
  }

let cgContextNew (): CirGenContext =
  {
    TyMap = HashMap()
    Stmts = ResizeArray()
    Decls = ResizeArray()
  }

let cgContextDerive (context: CirGenContext) =
  {
    context with
      Stmts = ResizeArray()
  }

let neverTerm = CName "__never__"

let kTyToCTy ty =
  match ty with
  | KIntTy ->
    CIntTy

  | KFunTy paramList ->
    let paramList =
      paramList |> List.map (fun (callBy, ty) ->
        match callBy with
        | ByMove ->
          ty |> kTyToCTy

        | ByRef ->
          ty |> kTyToCTy |> CPtrTy
      )
    CFunTy (paramList, CVoidTy)

let kPrimIsPure prim =
  match prim with
  | KAddPrim
  | KEqPrim ->
    true

  | KAssignPrim ->
    false

let kPrimToCBin prim =
  match prim with
  | KAddPrim ->
    CAddBin

  | KEqPrim ->
    CEqBin

  | KAssignPrim ->
    CAssignBin

let cgParam (context: CirGenContext) (KParam (callBy, arg)) =
  let ty =
    match context.TyMap.TryGetValue(arg) with
    | true, ty ->
      ty |> kTyToCTy

    | false, _ ->
      CIntTy

  match callBy with
  | ByMove ->
    CParam (arg, ty)

  | ByRef ->
    CParam (arg, CPtrTy ty)

let cgArg context (KArg (callBy, arg)) =
  let arg = cgTerm context arg

  match callBy with
  | ByMove ->
    arg

  | ByRef ->
    CUni (CRefUni, arg)

let cgTerm (context: CirGenContext) (node: KNode) =
  match node with
  | KInt text ->
    CInt text

  | KName name ->
    CName name

  | KPrim (prim, [first; second], result, next) ->
    let first = cgTerm context first
    let second = cgTerm context second

    let body = CBin (kPrimToCBin prim, first, second)
    let local = CLocalStmt (result, CIntTy, body)
    context.Stmts.Add(local)
    cgNode context next

    CName result

  | KApp (cal, args) ->
    let args = args |> List.map (cgArg context)

    let call = CTermStmt (CCall (CName cal, args))
    context.Stmts.Add(call)

    neverTerm

  | KFix (funName, paramList, body, next) ->
    let paramList = paramList |> List.map (cgParam context)

    let bodyContext = cgContextDerive context
    cgNode bodyContext body
    let body = bodyContext.Stmts |> Seq.toList

    let fnDecl = CFnDecl (funName, paramList, CVoidTy, body)
    context.Decls.Add(fnDecl)

    cgTerm context next

  | KPrim _ ->
    failwith "NEVER"

let cgNode context (node: KNode) =
  match node with
  | KInt _
  | KName _ ->
    ()

  | KPrim _
  | KApp _
  | KFix _ ->
    cgTerm context node |> ignore

let cirGen (node: KNode) =
  let tyContext = kirInfer node

  let context =
    { cgContextNew () with
        TyMap = tyContext.TyMap
    }

  cgNode context node
  context.Decls |> Seq.toList
