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

  | KPrim (prim, [first; second], result, next) ->
    let first = first |> keNode context
    let second = second |> keNode context

    let value =
      match prim with
      | KAddPrim ->
        match keDeref first, keDeref second with
        | KIntValue first, KIntValue second ->
          first + second |> KIntValue

        | _ ->
          failwith "(+) type error"

      | KEqPrim ->
        let value =
          if keDeref first = keDeref second then
            1
          else
            0

        KIntValue value

      | KAssignPrim ->
        match first with
        | KRefValue r ->
          r := keDeref second

        | _ ->
          failwith "(=) expected reference"

        first

      | KAssertEqPrim ->
        let first = first |> keDeref
        let second = second |> keDeref
        context.Output.AppendFormat("assert_eq({0}, {1})\n", first, second) |> ignore
        second

    context.Env <- context.Env |> Map.add result (ref value)
    next |> keNode context

  | KFix (name, paramList, body, next) ->
    let r = KFunValue (paramList, body, context.Env) |> ref
    context.Env <- context.Env |> Map.add name r
    r := KFunValue (paramList, body, context.Env)

    next |> keNode context

  | KApp (cal, args) ->
    let args = args |> List.map (keArg context)
    keCall context (fun () -> failwithf "can't call %s" cal) cal args

  | _ ->
    failwithf "can't evaluate %A" node

let rec kirEval (node: KNode) =
  let context = keContextNew ()
  node |> keNode context |> ignore
  keCall context (fun () -> context.Output.Append("No main function.") |> ignore; KIntValue 0) "main" [ref KExitValue] |> ignore
  context.Output.ToString()
