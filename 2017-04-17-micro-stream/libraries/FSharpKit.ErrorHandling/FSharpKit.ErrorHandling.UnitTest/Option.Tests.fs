namespace FSharpKit.ErrorHandling

open System
open FSharpKit.Misc.Disposables
open FSharpKit.UnitTest

module ``test Option`` =
  module ``test build`` =
    let ``test return`` =
      test {
        do!
          Option.build {
            return 1
          } |> is (Some 1)
      }

    let ``test return!`` =
      test {
        do!
          Option.build {
            return! Some 1
          } |> is (Some 1)
        do!
          Option.build {
            return! None
          } |> is None
      }

    module ``test use`` =
      let ``completion case`` =
        test {
          let disposable = new CountDisposable()
          do!
            Option.build {
              use disposable = disposable
              return disposable.Count
            } |> is (Some 0)
          do! disposable.Count |> is 1
        }

      let ``exceptional case`` =
        test {
          let disposable = new CountDisposable()
          let o () =
            Option.build {
              use disposable = disposable
              exn(disposable.Count |> string) |> raise
            }
          let! e = trap { it (o ()) }
          do! e.Message |> is "0"
          do! disposable.Count |> is 1
        }


    module ``test try-with`` =
      let ``completion case`` =
        test {
          let disposable = new CountDisposable()
          do!
            Option.build {
              try
                return disposable.Count
              with
              | _ ->
                disposable.Dispose()
                return! None
            } |> is (Some 0)
          do! disposable.Count |> is 0
        }

      let ``exceptional case`` =
        test {
          let disposable = new CountDisposable()
          let e =
            Option.build {
              try
                return! exn(disposable.Count |> string) |> raise
              with
              | e ->
                disposable.Dispose()
                return! Some e
            } |> Option.get
          do! e.Message |> is "0"
          do! disposable.Count |> is 1
        }

    module ``test try-finally`` =
      let ``completion case`` =
        test {
          let disposable = new CountDisposable()
          do!
            Option.build {
              try
                return disposable.Count
              finally
                disposable.Dispose()
            } |> is (Some 0)
          do! disposable.Count |> is 1
        }

      let ``exceptional case`` =
        test {
          let disposable = new CountDisposable()
          let o () =
            Option.build {
              try
                exn(disposable.Count |> string) |> raise
              finally
                disposable.Dispose()
            }
          let! e = trap { it (o ()) }
          do! e.Message |> is "0"
          do! disposable.Count |> is 1
        }

    module ``test while`` =
      let ``completion case`` =
        test {
          let i = ref 0
          let n = 5
          do!
            Option.build {
              while !i < n do
                i |> incr
            } |> is (Some ())
          do! !i |> is n
        }

      let ``error case`` =
        test {
          let i = ref 0
          let n = 5
          let tryIncrement () =
            let j = !i + 1
            if j < n then Some j else None
          do!
            Option.build {
              while true do
                let! j = tryIncrement ()
                i := j
            } |> is None
        }

    module ``test for`` =
      let ``completion case`` =
        test {
          let n = 5
          let xs = [|0..(n - 1)|]
          let list = ResizeArray()
          do!
            Option.build {
              for x in xs do
                list.Add(x)
            } |> is (Some ())
          do! list.ToArray() |> is xs
        }

      let ``error case`` =
        test {
          let n = 5
          let list = ResizeArray()
          do!
            Option.build {
              for x in seq {0..10000} do
                do! (if x < n then Some () else None)
                list.Add(x)
            } |> is None
          do! list.ToArray() |> is [|0..(n - 1)|]
        }
