namespace DotNetKit.FSharp.UnitTest

open System
open DotNetKit.FSharp.ErrorHandling
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection

module ResultTest =
  let ``test isOk and isError`` =
    let body (actual, expected) =
      test {
        do! actual |> Result.isOk |> assertEquals expected
        do! actual |> Result.isError |> not |> assertEquals expected
      }
    parameterize {
      case (Ok 0, true)
      case (Result.Error "x", false)
      run body
    }

  let ``test tryGet and tryGetError`` =
    let body (result, valueOption, errorOption) =
      test {
        do! result |> Result.tryGet |> assertEquals valueOption
        do! result |> Result.tryGetError |> assertEquals errorOption
      }
    parameterize {
      case (Ok 0, Some 0, None)
      case (Result.Error "x", None, Some "x")
      run body
    }

  let ``test getOr`` =
    test {
      do! Ok 1 |> Result.getOr 0 |> assertEquals 1
      do! Result.Error "x" |> Result.getOr 0 |> assertEquals 0
    }

  let ``test getErrorOr`` =
    test {
      do! Ok 1 |> Result.getErrorOr "x" |> assertEquals "x"
      do! Result.Error "y" |> Result.getErrorOr "x" |> assertEquals "y"
    }

  let ``test getOrElse`` =
    test {
      let count = ref 0
      let f () = count |> incr; 0
      do! Ok 1 |> Result.getOrElse f |> assertEquals 1
      do! !count |> assertEquals 0

      do! Result.Error "y" |> Result.getOrElse f |> assertEquals 0
      do! !count |> assertEquals 1
    }

  let ``test getErrorOrElse`` =
    test {
      let count = ref 0
      let f () = count |> incr; "x"
      do! Ok 1 |> Result.getErrorOrElse f |> assertEquals "x"
      do! !count |> assertEquals 1

      do! Result.Error "y" |> Result.getErrorOrElse f |> assertEquals "y"
      do! !count |> assertEquals 1
    }

  let ``test getOrThrow`` =
    test {
      do! Ok 1  |> Result.getOrThrow |> assertEquals 1
      let! e = trap { it (Result.Error "x" |> Result.getOrThrow) }
      do! e :? InvalidOperationException |> assertPred
    }

  let ``test getErrorOrThrow`` =
    test {
      let! e = trap { it (Ok 1 |> Result.getErrorOrThrow) }
      do! e :? InvalidOperationException |> assertPred
      do! Result.Error "x" |> Result.getErrorOrThrow |> assertEquals "x"
    }

  let ``test flatten`` =
    let body (result, expected) =
      test {
        do! result |> Result.flatten |> assertEquals expected
      }
    parameterize {
      case (Ok (Ok 0), Ok 0)
      case (Ok (Result.Error "x"), Result.Error "x")
      case (Result.Error "x", Result.Error "x")
      run body
    }

  let ``test flattenError`` =
    let body (result, expected) =
      test {
        do! result |> Result.flattenError |> assertEquals expected
      }
    parameterize {
      case (Ok 0, Ok 0)
      case (Result.Error (Ok 0), Ok 0)
      case (Result.Error (Result.Error "x"), Result.Error "x")
      run body
    }

  let ``test map`` =
    let body (result, expected) =
      test {
        do! result |> Result.map (fun x -> x + 1) |> assertEquals expected
      }
    parameterize {
      case (Ok 0, Ok 1)
      case (Result.Error "x", Result.Error "x")
      run body
    }

  let ``test mapError`` =
    let body (result, expected) =
      test {
        do! result |> Result.mapError (fun e -> e + "y") |> assertEquals expected
      }
    parameterize {
      case (Ok 0, Ok 0)
      case (Result.Error "x", Result.Error "xy")
      run body
    }

  let ``test bind`` =
    let body (result, expected) =
      test {
        let f x = if x >= 0 then Ok (x + 1) else Result.Error "x"
        do! result |> Result.bind f |> assertEquals expected
      }
    parameterize {
      case (Ok 0, Ok 1)
      case (Ok (-1), Result.Error "x")
      case (Result.Error "y", Result.Error "y")
      run body
    }

  let ``test bindError`` =
    let body (result, expected) =
      test {
        let f e = if e = "x" then Result.Error "y" else Ok 0
        do! result |> Result.bindError f |> assertEquals expected
      }
    parameterize {
      case (Ok 0, Ok 0)
      case (Result.Error "x", Result.Error "y")
      case (Result.Error "y", Ok 0)
      run body
    }

  let ``test exists and forall`` =
    let body (result, expectedExists, expectedForall) =
      test {
        let p x = x >= 0
        do! result |> Result.exists p |> assertEquals expectedExists
        do! result |> Result.forall p |> assertEquals expectedForall
      }
    parameterize {
      case (Ok 0, true, true)
      case (Ok (-1), false, false)
      case (Result.Error "x", false, true)
      run body
    }

  let ``test existsError and forallError`` =
    let body (result, expectedExists, expectedForall) =
      test {
        let p e = e = "x"
        do! result |> Result.existsError p |> assertEquals expectedExists
        do! result |> Result.forallError p |> assertEquals expectedForall
      }
    parameterize {
      case (Ok 0, false, true)
      case (Result.Error "x", true, true)
      case (Result.Error "y", false, false)
      run body
    }
