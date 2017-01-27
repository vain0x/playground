namespace Bracky.Runtime.Typing

open DotNetKit.FSharp.ErrorHandling
open FParsec
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Bracky.Runtime.Typing

module TypeExpressionBuilders =
  let tUnit = TypeExpression.unit
  let tBool = TypeExpression.bool
  let tInt = TypeExpression.int
  let tRef tv = RefTypeExpression tv
  let tFun s t = FunTypeExpression (s, t)
  let tApp k a = AppTypeExpression (k, a)

  let tx = TypeVariable.fresh ()
  let ty = TypeVariable.fresh ()
  let tz = TypeVariable.fresh ()

open TypeExpressionBuilders

module TypeExpressionTest =
  let ``test typeVariables`` =
    let body (t, expected) =
      test {
        do! t |> TypeExpression.typeVariables |> Set.ofSeq |> assertEquals (expected |> Set.ofList)
      }
    parameterize {
      case (tUnit, [])
      case (tRef tx, [tx])
      case (tFun (tRef tx) (tRef tx), [tx])
      case (tFun (tRef tx) (tFun tUnit (tRef ty)), [tx; ty])
      run body
    }

module SubstitutionTest =
  let ``test Extend and ExtendMany`` =
    let substitution1 =
      Substitution.Empty
        .Extend(tx, tUnit)
        .Extend(ty, tFun tInt (tRef tx))
    let substitution2 =
      let bindings =
        [|
          (tx, tUnit)
          (ty, tFun tInt (tRef tx))
        |]
      Substitution.Empty.ExtendMany(bindings)
    let body (t, expected) =
      test {
        do! substitution1.Apply(t) |> assertEquals expected
        do! substitution2.Apply(t) |> assertEquals expected
      }
    parameterize {
      case (tUnit, tUnit)
      case (tFun tInt tUnit, tFun tInt tUnit)
      case (tRef tx, tUnit)
      case (tRef ty, tFun tInt tUnit)
      run body
    }

module ``test Substitution`` =
  let ``test unify success`` =
    let body (t, t') =
      test {
        let substitution = Substitution.Empty |> Substitution.unify t t'
        do! substitution.Apply(t) |> assertEquals (substitution.Apply(t'))
      }
    parameterize {
      case (tInt, tInt)
      case (tInt, tRef tx)
      case (tRef tx, tRef tx)
      case (tRef tx, tRef ty)
      case (tFun tInt tUnit, tFun tInt tUnit)
      case (tFun tInt tUnit, tFun (tRef tx) (tRef ty))
      case (tFun (tRef ty) (tRef tx), tFun (tRef tx) (tRef ty))
      run body
    }

module ForallTypeSchemeTest =
  let ``test FreeTypeVariableSet`` =
    test {
      let ts = ForallTypeScheme ([| tx; ty |], tFun (tRef tx) (tFun (tRef ty) (tRef tz)))
      do! ts.FreeTypeVariableSet |> assertEquals (Set.singleton tz)
    }

  let ``test Instantiate`` =
    test {
      let ts = ForallTypeScheme ([| tx; ty |], tFun (tRef tx) (tFun (tRef ty) (tRef tz)))
      match ts.Instantiate() with
      | FunTypeExpression
        ( RefTypeExpression tx'
        , (FunTypeExpression (RefTypeExpression ty', RefTypeExpression tz'))
        ) ->
        do! [tx; ty; tz; tx'; ty'] |> set |> Set.count |> assertEquals 5
        do! tz' |> assertEquals tz
      | _ ->
        return! fail "Some variable isn't fresh."
    }

module TypeInfererTest =
  open Bracky.Runtime.Parsing
  open Bracky.Runtime.Parsing.ExpressionBuilders

  let ``test infer success`` =
    let body (source, expected) =
      test {
        match Parsers.parseExpression "test" source with
        | Result.Ok expression ->
          let tv = TypeVariable.fresh ()
          let inferer = TypeInferer.empty |> TypeInferer.infer expression (tRef tv)
          let substitution = inferer |> TypeInferer.substitution
          do! substitution.Apply(tRef tv) |> assertEquals expected
        | Result.Error message ->
          return! fail message
      }
    parameterize {
      case ("0", tInt)
      case ("true", tBool)
      case ("1 + 2", tInt)
      case ("{fun x -> x + 1}", tFun tInt tInt)
      case ("{fun x -> 2 * x}", tFun tInt tInt)
      case ("val x = 2", tUnit)
      case ("val x = 2; x", tInt)
      case ("{fun x -> x; 2}", tFun tUnit tInt)
      case ("val id = {fun x -> x}; id (val x = id 0)", tUnit)
      case ("{if true -> 1; else 0}", tInt)
      case ("{if true -> (val x = 0)}", tUnit)
      run body
    }

  let ``test infer failure`` =
    let body source =
      test {
        match Parsers.parseExpression "test" source with
        | Result.Ok expression ->
          let tv = TypeVariable.fresh ()
          let run () =
            TypeInferer.empty |> TypeInferer.infer expression (tRef tv)
          let! e = trap { it (run ()) }
          return ()
        | Result.Error message ->
          return! fail message
      }
    parameterize {
      case "x"
      case "{fun x -> val y = 0} + 0"
      case "{if 1 -> then; else false}"
      case "{if true -> 1; else false}"
      case "{if true -> 1}"
      run body
    }