namespace FSharpKit.ErrorHandling

open System
open FSharpKit.Misc.Disposables
open FSharpKit.UnitTest

module ``test Result`` =
  let ``test isOk and isError`` =
    let body (actual, expected) =
      test {
        do! actual |> Result.isOk |> is expected
        do! actual |> Result.isError |> not |> is expected
      }
    parameterize {
      case (Ok 0, true)
      case (Error "x", false)
      run body
    }

  let ``test tryValue and tryError`` =
    let body (result, valueOption, errorOption) =
      test {
        do! result |> Result.tryValue |> is valueOption
        do! result |> Result.tryError |> is errorOption
      }
    parameterize {
      case (Ok 0, Some 0, None)
      case (Error "x", None, Some "x")
      run body
    }

  let ``test defaultValue`` =
    test {
      do! Ok 1 |> Result.defaultValue 0 |> is 1
      do! Error "x" |> Result.defaultValue 0 |> is 0
    }

  let ``test defaultError`` =
    test {
      do! Ok 1 |> Result.defaultError "x" |> is "x"
      do! Error "y" |> Result.defaultError "x" |> is "y"
    }

  let ``test defaultWith`` =
    test {
      let count = ref 0
      let f () = count |> incr; 0
      do! Ok 1 |> Result.defaultWith f |> is 1
      do! !count |> is 0

      do! Error "y" |> Result.defaultWith f |> is 0
      do! !count |> is 1
    }

  let ``test defaultWithError`` =
    test {
      let count = ref 0
      let f () = count |> incr; "x"
      do! Ok 1 |> Result.defaultWithError f |> is "x"
      do! !count |> is 1

      do! Error "y" |> Result.defaultWithError f |> is "y"
      do! !count |> is 1
    }

  let ``test valueOrRaise`` =
    test {
      do! Ok 1 |> Result.valueOrRaise |> is 1
      let! e = trap { it (Error "x" |> Result.valueOrRaise) }
      do! e :? InvalidOperationException |> is true
    }

  let ``test errorOrRaise`` =
    test {
      let! e = trap { it (Ok 1 |> Result.errorOrRaise) }
      do! e :? InvalidOperationException |> is true
      do! Error "x" |> Result.errorOrRaise |> is "x"
    }

  let ``test flatten`` =
    let body (result, expected) =
      test {
        do! result |> Result.flatten |> is expected
      }
    parameterize {
      case (Ok (Ok 0), Ok 0)
      case (Ok (Error "x"), Error "x")
      case (Error "x", Error "x")
      run body
    }

  let ``test flattenError`` =
    let body (result, expected) =
      test {
        do! result |> Result.flattenError |> is expected
      }
    parameterize {
      case (Ok 0, Ok 0)
      case (Error (Ok 0), Ok 0)
      case (Error (Error "x"), Error "x")
      run body
    }

  let ``test bindError`` =
    let body (result, expected) =
      test {
        let f e = if e = "x" then Error "y" else Ok 0
        do! result |> Result.bindError f |> is expected
      }
    parameterize {
      case (Ok 0, Ok 0)
      case (Error "x", Error "y")
      case (Error "y", Ok 0)
      run body
    }

  let ``test iter`` =
    let body (result, expectedValues, expectedErrors) =
      test {
        // Test iter.
        let values = ResizeArray()
        result |> Result.iter values.Add
        do! values.ToArray() |> is expectedValues

        // Test iterError.
        let errors = ResizeArray()
        result |> Result.iterError errors.Add
        do! errors.ToArray() |> is expectedErrors
      }
    parameterize {
      case (Ok 0, [|0|], [||])
      case (Error "x", [||], [|"x"|])
      run body
    }

  let ``test exists and forall`` =
    let body (result, expectedExists, expectedForall) =
      test {
        let p x = x >= 0
        do! result |> Result.exists p |> is expectedExists
        do! result |> Result.forall p |> is expectedForall
      }
    parameterize {
      case (Ok 0, true, true)
      case (Ok (-1), false, false)
      case (Error "x", false, true)
      run body
    }

  let ``test existsError and forallError`` =
    let body (result, expectedExists, expectedForall) =
      test {
        let p e = e = "x"
        do! result |> Result.existsError p |> is expectedExists
        do! result |> Result.forallError p |> is expectedForall
      }
    parameterize {
      case (Ok 0, false, true)
      case (Error "x", true, true)
      case (Error "y", false, false)
      run body
    }

  module ``test build`` =
    let ``test return`` =
      test {
        do!
          Result.build {
            return 1
          } |> is (Ok 1)
      }

    let ``test return!`` =
      test {
        do!
          Result.build {
            return! Ok 1
          } |> is (Ok 1)
        do!
          Result.build {
            return! Error "x"
          } |> is (Error "x")
      }

    module ``test use`` =
      let ``completion case`` =
        test {
          let disposable = new CountDisposable()
          do!
            Result.build {
              use disposable = disposable
              return disposable.Count
            } |> is (Ok 0)
          do! disposable.Count |> is 1
        }

      let ``exceptional case`` =
        test {
          let disposable = new CountDisposable()
          let r () =
            Result.build {
              use disposable = disposable
              exn(disposable.Count |> string) |> raise
            }
          let! e = trap { it (r ()) }
          do! e.Message |> is "0"
          do! disposable.Count |> is 1
        }


    module ``test try-with`` =
      let ``completion case`` =
        test {
          let disposable = new CountDisposable()
          do!
            Result.build {
              try
                return disposable.Count
              with
              | e ->
                disposable.Dispose()
                return! Error e
            } |> is (Ok 0)
          do! disposable.Count |> is 0
        }

      let ``exceptional case`` =
        test {
          let disposable = new CountDisposable()
          let e =
            Result.build {
              try
                exn(disposable.Count |> string) |> raise
              with
              | e ->
                disposable.Dispose()
                return! Error e
            } |> Result.errorOrRaise
          do! e.Message |> is "0"
          do! disposable.Count |> is 1
        }

    module ``test try-finally`` =
      let ``completion case`` =
        test {
          let disposable = new CountDisposable()
          do!
            Result.build {
              try
                return disposable.Count
              finally
                disposable.Dispose()
            } |> is (Ok 0)
          do! disposable.Count |> is 1
        }

      let ``exceptional case`` =
        test {
          let disposable = new CountDisposable()
          let r () =
            Result.build {
              try
                exn(disposable.Count |> string) |> raise
              finally
                disposable.Dispose()
            }
          let! e = trap { it (r ()) }
          do! e.Message |> is "0"
          do! disposable.Count |> is 1
        }

    module ``test while`` =
      let ``completion case`` =
        test {
          let i = ref 0
          let n = 5
          do!
            Result.build {
              while !i < n do
                i |> incr
            } |> is (Ok ())
          do! !i |> is n
        }

      let ``error case`` =
        test {
          let i = ref 0
          let n = 5
          let tryIncrement () =
            let j = !i + 1
            if j < n then Ok j else Error ()
          do!
            Result.build {
              while true do
                let! j = tryIncrement ()
                i := j
            } |> is (Error ())
        }

    module ``test for`` =
      let ``completion case`` =
        test {
          let n = 5
          let xs = [|0..(n - 1)|]
          let list = ResizeArray()
          do!
            Result.build {
              for x in xs do
                list.Add(x)
            } |> is (Ok ())
          do! list.ToArray() |> is xs
        }

      let ``error case`` =
        test {
          let n = 5
          let list = ResizeArray()
          do!
            Result.build {
              for x in seq {0..10000} do
                do! (if x < n then Ok () else Error x)
                list.Add(x)
            } |> is (Error n)
          do! list.ToArray() |> is [|0..(n - 1)|]
        }
