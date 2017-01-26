namespace Bracky.Runtime.Typing

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
  let ``test TypeVariableSet`` =
    let body (t, expected) =
      test {
        do! (t: TypeExpression).TypeVariableSet |> assertEquals expected
      }
    parameterize {
      case (tUnit, Set.empty)
      case (tRef tx, Set.singleton tx)
      case (tFun (tRef tx) (tRef tx), Set.singleton tx)
      case (tFun (tRef tx) (tFun tUnit (tRef ty)), set [tx; ty])
      run body
    }

module SubstitutionTest =
  let ``test Extend and ExtendMany`` =
    let substitution1 =
      Substitution.Empty
        .Extend(tx, tUnit)
        .Extend(ty, tFun tInt (tRef tx))
    let substitution2 =
      let map =
        [
          (tx, tUnit)
          (ty, tFun tInt (tRef tx))
        ]
      Substitution.Empty.ExtendMany(Map.ofList map)
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

module TypeInferenceTest =
  let ``test unify success`` =
    let body (t, t') =
      test {
        let substitution = Substitution.Empty |> TypeInference.unify t t'
        do! substitution.Apply(t) |> assertEquals (substitution.Apply(t'))
      }
    parameterize {
      case (tInt, tInt)
      case (tInt, tRef tx)
      case (tRef tx, tRef tx)
      case (tRef tx, tRef ty)
      case (tFun tInt tUnit, tFun tInt tUnit)
      case (tFun tInt tUnit, tFun (tRef tx) (tRef ty))
      //case (tFun (tRef ty) (tRef tx), tFun (tRef tx) (tRef ty))
      // Substitution happens to map tRef ty to tRef ty and causes a stack overflow.
      run body
    }

  open Bracky.Runtime.Parsing
  open Bracky.Runtime.Parsing.ExpressionBuilders

  let ``test infer success`` =
    let body (source, expected) =
      test {
        match Parsers.parseExpression "test" source with
        | Ok expression ->
          let tv = TypeVariable.fresh ()
          let (substitution, environment) =
            TypeInference.infer expression (tRef tv) Substitution.Empty Map.empty
          do! substitution.Apply(tRef tv) |> assertEquals expected
        | Error message ->
          return! fail message
      }
    parameterize {
      case ("0", tInt)
      case ("true", tBool)
      case ("val x = 2", tUnit)
      case ("val x = 2; x", tInt)
      case ("{fun x -> x; 2}", tFun tUnit tInt)
      case ("val id = {fun x -> x}; id (val x = id 0)", tUnit)
      run body
    }
