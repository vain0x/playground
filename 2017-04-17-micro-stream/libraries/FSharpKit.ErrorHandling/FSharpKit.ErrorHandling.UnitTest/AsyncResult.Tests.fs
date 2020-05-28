namespace FSharpKit.ErrorHandling

open FSharpKit.Misc.Disposables
open FSharpKit.UnitTest
open System.Threading

module ``test AsyncResult`` =
  let run a = a |> Async.RunSynchronously

  module ``test build`` =
    let positive x =
      async {
        do! Async.SwitchToThreadPool()
        return
          if x >= 1
          then Ok x
          else x |> string |> Error
      }

    let ``test return`` =
      test {
        do! AsyncResult.build { return 1 } |> run |> is (Ok 1)
      }

    let ``test return!`` =
      test {
        do! AsyncResult.build { return! positive 1 } |> run |> is (Ok 1)
        do! AsyncResult.build { return! positive 0 } |> run |> is (Error "0")
      }

    let ``test let!: Success case.`` =
      test {
        do!
          AsyncResult.build {
            let! x = positive 1
            let! y = positive 2
            let! z = Ok 3
            return x + y + z
          } |> run |> is (Ok 6)
      }

    let ``test let!: Error case.`` =
      test {
        do!
          AsyncResult.build {
            let! x = positive 1
            let! y = positive 0
            exn() |> raise
            return x + y
          } |> run |> is (Error "0")
      }

    let ``test do!`` =
      test {
        do!
          AsyncResult.build {
            // On the main thread.
            let threadId = Thread.CurrentThread.ManagedThreadId
            do! Async.SwitchToThreadPool()
            // On a thread from the thread pool.
            if threadId = Thread.CurrentThread.ManagedThreadId then
              return! Error "Unexpectedly on the main thread." |> AsyncResult.ofResult
          } |> run |> is (Ok ())
      }

    module ``test use`` =
      let ``completion case`` =
        test {
          let disposable = new CountDisposable()
          let ar =
            AsyncResult.build {
              use disposable = disposable
              do! Async.SwitchToThreadPool()
              return disposable.Count
            }
          do! disposable.Count |> is 0
          let result = ar |> run
          do! (result, disposable.Count) |> is (Ok 0, 1)
        }

      let ``exceptional case`` =
        test {
          let disposable = new CountDisposable()
          let ar =
            AsyncResult.build {
              use disposable = disposable
              do! Async.SwitchToThreadPool()
              exn(disposable.Count |> string) |> raise
              return disposable.Count
            }
          do! disposable.Count |> is 0
          let result =
            try
              ar |> run
            with
            | e -> Error e
          let k = (result |> Result.errorOrRaise).Message |> int
          do! (k, disposable.Count) |> is (0, 1)
        }

    module ``test try-with`` =
      let ``completion case`` =
        test {
          let disposable = new CountDisposable()
          let ar =
            AsyncResult.build {
              try
                do! Async.SwitchToThreadPool()
                return disposable.Count
              with
              | e ->
                disposable.Dispose()
                return! e |> AsyncResult.error
            }
          do! disposable.Count |> is 0
          let result = ar |> run
          do! (result, disposable.Count) |> is (Ok 0, 0)
        }

      let ``exceptional case`` =
        test {
          let disposable = new CountDisposable()
          let ar =
            AsyncResult.build {
              try
                do! Async.SwitchToThreadPool()
                return exn(disposable.Count |> string) |> raise
              with
              | e ->
                disposable.Dispose()
                return! e |> AsyncResult.error
            }
          do! disposable.Count |> is 0
          let result =
            try
              ar |> run
            with
            | e -> Error e
          let k = (result |> Result.errorOrRaise).Message |> int
          do! (k, disposable.Count) |> is (0, 1)
        }

    module ``test try-finally`` =
      let ``completion case`` =
        test {
          let disposable = new CountDisposable()
          let ar =
            AsyncResult.build {
              try
                do! Async.SwitchToThreadPool()
                return disposable.Count
              finally
                disposable.Dispose()
            }
          do! disposable.Count |> is 0
          let result = ar |> run
          do! (result, disposable.Count) |> is (Ok 0, 1)
        }

      let ``exceptional case`` =
        test {
          let disposable = new CountDisposable()
          let ar =
            AsyncResult.build {
              try
                do! Async.SwitchToThreadPool()
                return exn(disposable.Count |> string) |> raise
              finally
                disposable.Dispose()
            }
          do! disposable.Count |> is 0
          let result =
            try
              ar |> run
            with
            | e -> Error e
          let k = (result |> Result.errorOrRaise).Message |> int
          do! (k, disposable.Count) |> is (0, 1)
        }

    let ``test Combine`` =
      test {
        let count = ref 0
        do!
          AsyncResult.build {
            count |> incr
            return !count
          } |> run |> is (Ok 1)
      }

    module ``test while`` =
      let ``completion case`` =
        test {
          let count = ref 0
          let n = 10
          let counts = ResizeArray()
          let incr =
            async {
              counts.Add(!count)
              count |> incr
              return Ok ()
            }
          do!
            AsyncResult.build {
              while (!count) < n do
                do! incr
            } |> run |> is (Ok ())
          do! !count |> is n
          do! counts.ToArray() |> is [|0..(n - 1)|]
        }

      let ``error case`` =
        test {
          let n = 3
          let tryIncrement x =
            async {
              let y = x + 1
              return if y < n then Ok y else Error ()
            }
          let i = ref 0
          let values = ResizeArray()
          do!
            AsyncResult.build {
              while true do
                values.Add(!i)
                let! j = !i |> tryIncrement
                i := j
            } |> run |> is (Error ())
          do! values.ToArray() |> is [|0..(n - 1)|]
        }

    module ``test for`` =
      let ``completion case`` =
        test {
          let n = 5
          let advanceCount = ref 0
          let xs =
            seq {
              for i in 0..(n - 1) do
                advanceCount |> incr
                yield i
            }
          let values = ResizeArray()
          do!
            AsyncResult.build {
              for x in xs do
                values.Add((x, !advanceCount))
            } |> run |> is (Ok ())
          do! values.ToArray() |> is [|for i in 0..(n - 1) -> (i, i + 1)|]
        }
