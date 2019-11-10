namespace Bracky.Runtime.Typing

open FParsec
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Bracky.Runtime.Typing

module TypeExpressionBuilders =
  let tUnit =
    TypeExpression.unit

  let tBool =
    TypeExpression.bool

  let tInt =
    TypeExpression.int

  let tVar tv =
    VarTypeExpression tv

  let tFun sourceType targetType =
    FunTypeExpression (sourceType, targetType)

  let tKind kind types =
    KindTypeExpression (kind, types)

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
      case (tVar tx, [tx])
      case (tFun (tVar tx) (tVar tx), [tx])
      case (tFun (tVar tx) (tFun tUnit (tVar ty)), [tx; ty])
      run body
    }

module SubstitutionTest =
  let ``test Extend and ExtendMany`` =
    let substitution1 =
      Substitution.Empty
        .Extend(tx, tUnit)
        .Extend(ty, tFun tInt (tVar tx))
    let substitution2 =
      let bindings =
        [|
          (tx, tUnit)
          (ty, tFun tInt (tVar tx))
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
      case (tVar tx, tUnit)
      case (tVar ty, tFun tInt tUnit)
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
      case (tInt, tVar tx)
      case (tVar tx, tVar tx)
      case (tVar tx, tVar ty)
      case (tFun tInt tUnit, tFun tInt tUnit)
      case (tFun tInt tUnit, tFun (tVar tx) (tVar ty))
      case (tFun (tVar ty) (tVar tx), tFun (tVar tx) (tVar ty))
      run body
    }

module TypeSchemeTest =
  let ``test FreeTypeVariableSet`` =
    test {
      let ts = TypeScheme ([| tx; ty |], tFun (tVar tx) (tFun (tVar ty) (tVar tz)))
      do! ts.FreeTypeVariableSet |> assertEquals (Set.singleton tz)
    }

  let ``test Instantiate`` =
    test {
      let ts = TypeScheme ([| tx; ty |], tFun (tVar tx) (tFun (tVar ty) (tVar tz)))
      match ts.Instantiate() with
      | FunTypeExpression
        ( VarTypeExpression tx'
        , (FunTypeExpression (VarTypeExpression ty', VarTypeExpression tz'))
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
          let inferer = TypeInferer.empty |> TypeInferer.infer expression (tVar tv)
          let substitution = inferer |> TypeInferer.substitution
          do! substitution.Apply(tVar tv) |> assertEquals expected
        | Result.Error message ->
          return! fail message
      }
    parameterize {
      // literal
      case ("()", tUnit)
      case ("0", tInt)
      case ("true", tBool)
      case ("1 + 2", tInt)
      // val
      case ("{val () = ()}", tUnit)
      case ("{val () = {val x = 0}}", tUnit)
      case ("{val x = 2}", tUnit)
      case ("{val x = 2}; x", tInt)
      case ("{val f x = x + 1}; f", tFun tInt tInt)
      case ("{val f x y = (x; y)}; f () 1", tInt)
      // fun
      case ("{fun () -> 1}", tFun tUnit tInt)
      case ("{fun x -> x; 2}", tFun tUnit tInt)
      case ("{fun x -> x + 1}", tFun tInt tInt)
      case ("{fun x -> 2 * x}", tFun tInt tInt)
      // if
      case ("{if true -> 1; else 0}", tInt)
      case ("{if true -> {val x = 0}}", tUnit)
      // apply
      case ("{fun () -> 1} ()", tInt)
      case ("{val id = {fun x -> x}}; id (); id 0", tInt)
      run body
    }

  let ``test infer failure`` =
    let body source =
      test {
        match Parsers.parseExpression "test" source with
        | Result.Ok expression ->
          let tv = TypeVariable.fresh ()
          let run () =
            TypeInferer.empty |> TypeInferer.infer expression (tVar tv)
          let! e = trap { it (run ()) }
          return ()
        | Result.Error message ->
          return! fail message
      }
    parameterize {
      // literals
      case "x"
      // if
      case "{if 1 -> true; else false}"
      case "{if true -> 1; else false}"
      case "{if true -> 1}"
      // apply
      case "{fun () -> 1} 0"
      // add
      case "() + ()"
      case "() + 0"
      case "() * 0"
      run body
    }
