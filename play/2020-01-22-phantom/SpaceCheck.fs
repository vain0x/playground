module rec SpaceCheck

open Core
open SpaceExclude

let mapKeys map =
  map |> Map.toList |> List.map fst

type SpaceCheckContext =
  {
    Env: Map<string, Space>
    Ensures: Ensure list
  }

type Scc = SpaceCheckContext

let sccEmpty =
  {
    Env = Map.empty
    Ensures = []
  }

let sccGetSpace local (context: Scc) =
  context.Env |> Map.tryFind local |> Option.defaultValue spaceEmpty

// let sccUnion (first: Scc) (second: Scc) =
//   let locals = first.Env |> mapKeys |> List.append (second.Env |> mapKeys) |> set |> Set.toList

//   let env =
//     locals |> List.fold (fun env local ->
//       match first.Env |> Map.tryFind local, second.Env |> Map.tryFind local with
//       | Some first, Some second ->
//         env |> Map.add local (UnionSpace [first; second])

//       | Some first, None ->
//         env |> Map.add local first

//       | None, Some second ->
//         env |> Map.add local second

//       | None, None ->
//         assert false
//         env
//     ) Map.empty

//   {
//     Env = env
//     Ensure = []
//   }

let sccAddLocal local space context =
  { context with
      Env = context.Env |> Map.add local space
  }

let sccDefineLocal (local: Local) context =
  context |> sccAddLocal local.LocalName (TySpace local.Ty)

let constrSubst map constr =
  match constr with
  | CaseConstr (KName local, pat) ->
    match map |> Map.tryFind local.LocalName with
    | Some term ->
      // FIXME: subst pat
      CaseConstr (term, pat)

    | None ->
      failwithf "unknown %s in constr(%A)" local.LocalName constr

  | CaseConstr _ ->
    constr

let ensureSubst map (pre, post) =
  constrSubst map pre, constrSubst map post

let sccAddEnsures (args: KTerm list) (result: Local) (fn: Fn) context: Scc =
  let map =
    List.zip (fn.Params |> List.map (fun param -> param.LocalName)) args
    |> Map.ofList

  let map =
    map |> Map.add fn.Result.LocalName (KName result)

  let ensures =
    fn.Ensures |> List.map (ensureSubst map)

  { context with
      Ensures = ensures @ context.Ensures
  }

let sccUseEnsures (context: Scc) =
  let ensures = context.Ensures

  let ensures, context =
    ensures |> List.fold (fun (ensures, context) ensure ->
      let pre, post = ensure

      // if pre
      match pre with
      | CaseConstr (term, pat) ->
        let termSpace = scTerm (term, context)
        let patSpace = pat |> patToSpace
        let leak = spaceExclude termSpace patSpace
        if leak |> spaceIsEmpty |> not then
          ensure :: ensures, context
        else
          // assume post
          match post with
          | CaseConstr (KName local, pat) ->
            let localSpace = context |> sccGetSpace local.LocalName
            let patSpace = pat |> patToSpace
            let restricted = spaceIntersect localSpace patSpace
            let context = context |> sccAddLocal local.LocalName restricted
            ensures, context

          | _ ->
            ensures, context
    ) ([], context)

  { context with Ensures = ensures }

let scTypeCheck context term ty =
  let go context =
    let termSpace = scTerm (term, context)
    let tySpace = TySpace ty

    let leak = spaceExclude termSpace tySpace
    if leak |> spaceIsEmpty then
      Ok ()
    else
      Error leak

  match go context with
  | Ok () ->
    ()

  | Error _ ->

  let context = context |> sccUseEnsures
  match go context with
  | Ok () ->
    ()

  | Error leak ->
    failwithf "term(%A) not of (%A) e.g. %A" term ty (leak |> spaceToAnyPat)

let scEnsuresCheck context (ensures: Ensure list) =
  let context = context |> sccUseEnsures

  for pre, post in ensures do
    match post with
    | CaseConstr (term, pat) ->
      let termSpace = scTerm (term, context)
      let patSpace = pat |> patToSpace

      // post が成立しないスペース
      let badSpace = spaceExclude termSpace patSpace

      match pre with
      | CaseConstr (term, pat) ->
        let termSpace = scTerm (term, context)
        let patSpace = pat |> patToSpace

        // pre が成立しないスペース
        let conditionedSpace = spaceExclude termSpace patSpace

        // pre が成立して post が成立しないスペース
        let remain = spaceExclude badSpace conditionedSpace
        if remain |> spaceIsEmpty |> not then
          failwithf "term(%A) !@ %A e.g. %A" term pat (remain |> spaceToAnyPat)

let scTerm (term, context) =
  match term with
  | KInt value ->
    spaceFromInt value

  | KName local ->
    context |> sccGetSpace local.LocalName

  | KSelf ->
    failwithf "self は phantom/phase 宣言以外で使えません"

let scNode (currentFn: Fn) (node, context) =
  match node with
  | KReturn term ->
    // FIXME: 結果の型検査

    let resultSpace = scTerm (term, context)
    let context = context |> sccAddLocal currentFn.Result.LocalName resultSpace
    scEnsuresCheck context currentFn.Ensures

  | KJump (fn, args) ->
    // 引数の型検査
    for param, arg in List.zip fn.Params args do
      scTypeCheck context arg param.Ty

  | KCall (fn, args, result, next) ->
    // 引数の型検査
    for param, arg in List.zip fn.Params args do
      scTypeCheck context arg param.Ty

    // FIXME: 結果の型検査

    let context = context |> sccDefineLocal result
    let context = context |> sccAddEnsures args result fn

    scNode currentFn (next, context)

  | KCase (cond, pat, body, alt) ->
    match cond with
    | KName local ->
      let condSpace = scTerm (cond, context)
      let patSpace = pat |> patToSpace

      let altSpace = spaceExclude condSpace patSpace
      let bodySpace = spaceExclude condSpace altSpace

      let bodyContext = context |> sccAddLocal local.LocalName bodySpace
      scNode currentFn (body, bodyContext)

      let altContext = context |> sccAddLocal local.LocalName altSpace
      scNode currentFn (alt, altContext)

    | _ ->
      scNode currentFn (body, context)
      scNode currentFn (alt, context)

let scFn (fn: Fn, context) =
  let context =
    fn.Params |> List.fold (fun context local ->
      context |> sccDefineLocal local
    ) context

  scNode fn (fn.Body, context)

let spaceCheck fn =
  let context = sccEmpty
  scFn (fn, context)
