module rec RaiiLang.KirEval

open System.Text
open RaiiLang.Helpers
open RaiiLang.Kir

type KEnv = Map<string, KValue ref>

type KValue =
  | KBoolValue
    of boolValue:bool

  | KIntValue
    of intValue:int

  | KStrValue
    of strValue:string

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

let unitValue = KIntValue 0

let keContextNew (): KirEvalContext =
  {
    Env = Map.empty
    Output = StringBuilder()
  }

let keDeref arg =
  match arg with
  | KRefValue arg ->
    !arg |> keDeref

  | _ ->
    arg

let keRef arg =
  match arg with
  | KRefValue arg ->
    match !arg with
    | KRefValue _ as arg ->
      arg |> keRef

    | _ ->
      arg

  | _ ->
    ref arg

let keArg (context: KirEvalContext) (KArg (passBy, arg)) =
  let arg = arg |> keNode context

  match passBy with
  | ByMove ->
    // FIXME: move out
    arg |> keDeref

  | ByIn
  | ByRef ->
    arg

let kePrim context prim args =
  match prim, args with
  | KAddPrim, [KIntValue first; KIntValue second] ->
    first + second |> KIntValue

  | KEqPrim, [KRefValue first; KRefValue second] ->
    let value = if !first = !second then 1 else 0
    value |> KIntValue

  | KAssignPrim, [KRefValue first; second] ->
    first := second
    KRefValue first

  | KExternFnPrim "assert_eq", [KRefValue first; KRefValue second] ->
    context.Output.AppendFormat("assert_eq({0}, {1})\n", !first, !second) |> ignore

    unitValue

  | KExternFnPrim "print", [KRefValue first] ->
    match !first with
    | KStrValue text ->
      context.Output.AppendFormat("{0}\n", text) |> ignore

    | _ ->
      context.Output.Append(sprintf "%A\n" !first) |> ignore

    unitValue

  | KExternFnPrim "string_clone", [KRefValue first] ->
    !first

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
          env |> Map.add param (arg |> keRef)
        ) funEnv

      context.Env <- funEnv
      body |> keNode context

    | KExitValue ->
      KIntValue 0

    | _ ->
      onError ()

  | false, _ ->
    onError ()

let keNode context (node: KNode): KValue =
  match node with
  | KBool value ->
    value |> KBoolValue |> ref |> KRefValue

  | KInt text ->
    // FIXME: in 1 を動作させるために参照を挟む。
    text |> int |> KIntValue |> ref |> KRefValue

  | KStr segments ->
    segments
    |> List.map (fun (StrVerbatim text) -> text)
    |> String.concat ""
    |> KStrValue
    |> ref
    |> KRefValue

  | KName name ->
    match context.Env.TryGetValue(name) with
    | true, r ->
      KRefValue r

    | false, _ ->
      failwithf "name '%s' not found" name

  | KPrim (prim, args, res, next) ->
    let args = args |> List.map (keArg context)

    let value = kePrim context prim args
    context.Env <- context.Env |> Map.add res (ref value)

    next |> keNode context

  | KApp (cal, args) ->
    let args = args |> List.map (keArg context)

    keCall context (fun () -> failwithf "can't call %s" cal) cal args

  | KFix (name, paramList, body, next) ->
    let r = KFunValue (paramList, body, context.Env) |> ref
    context.Env <- context.Env |> Map.add name r
    r := KFunValue (paramList, body, context.Env)

    next |> keNode context

let rec kirEval (node: KNode) =
  let context = keContextNew ()

  node |> keNode context |> ignore

  keCall context (fun () ->
    context.Output.Append("No main function.") |> ignore
    KIntValue 0
  ) "main" [KExitValue] |> ignore

  context.Output.ToString()
