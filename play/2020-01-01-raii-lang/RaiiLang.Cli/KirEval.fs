module rec RaiiLang.KirEval

open System.Text
open RaiiLang.Helpers
open RaiiLang.Kir

type KEnv = Map<string, KValue ref>

type KValue =
  | KIntValue
    of intValue:int

  | KRefValue
    of refValue:KValue ref

  | KFunValue
    of KParam list * body:KNode * env:KEnv

  | KExitValue

type KirEvalContext =
  {
    mutable Env: KEnv
    Output: StringBuilder
  }

let keContextNew (): KirEvalContext =
  {
    Env = Map.empty
    Output = StringBuilder()
  }

let keDeref value =
  match value with
  | KRefValue r ->
    !r |> keDeref

  | _ ->
    value

let keArg (context: KirEvalContext) (KArg (callBy, arg)) =
  let arg = arg |> keNode context

  match callBy, arg with
  | ByMove, _ ->
    arg |> keDeref |> ref

  | ByRef, KRefValue r ->
    r

  | ByRef, _ ->
    failwithf "expected a reference but %A" arg

let kePrim context prim args =
  match prim, args with
  | KAddPrim, [first; second] ->
    match !first, !second with
    | KIntValue first, KIntValue second ->
      first + second |> KIntValue |> ref

    | _ ->
      failwith "(+) type error"

  | KEqPrim, [first; second] ->
    let value =
      if first = second || !first = !second then
        1
      else
        0

    value |> KIntValue |> ref

  | KAssignPrim, [first; second] ->
    first := !second
    first

  | KExternFnPrim "assert_eq", [first; second] ->
    context.Output.AppendFormat("assert_eq({0}, {1})\n", !first, !second) |> ignore
    first

  | _ ->
    failwithf "can't evaluate prim %A" (prim, args)

let keCall context onError cal args =
  match context.Env.TryGetValue(cal) with
  | true, r ->
    match !r with
    | KFunValue (paramList, body, funEnv) ->
      let funEnv =
        Seq.zip paramList args
        |> Seq.fold (fun env (KParam (_, param), arg) ->
          env |> Map.add param arg
        ) funEnv

      context.Env <- funEnv
      body |> keNode context

    | KExitValue ->
      KIntValue 0

    | _ ->
      onError ()

  | false, _ ->
    onError ()

let keNode context (node: KNode) =
  match node with
  | KInt text ->
    text |> int |> KIntValue

  | KName name ->
    match context.Env.TryGetValue(name) with
    | true, r ->
      KRefValue r

    | false, _ ->
      failwithf "name '%s' not found" name

  | KPrim (prim, args, res, next) ->
    let args = args |> List.map (keArg context)

    let value = kePrim context prim args
    context.Env <- context.Env |> Map.add res value

    next |> keNode context

  | KFix (name, paramList, body, next) ->
    let r = KFunValue (paramList, body, context.Env) |> ref
    context.Env <- context.Env |> Map.add name r
    r := KFunValue (paramList, body, context.Env)

    next |> keNode context

  | KApp (cal, args) ->
    let args = args |> List.map (keArg context)
    keCall context (fun () -> failwithf "can't call %s" cal) cal args

let rec kirEval (node: KNode) =
  let context = keContextNew ()
  node |> keNode context |> ignore
  keCall context (fun () -> context.Output.Append("No main function.") |> ignore; KIntValue 0) "main" [ref KExitValue] |> ignore
  context.Output.ToString()
