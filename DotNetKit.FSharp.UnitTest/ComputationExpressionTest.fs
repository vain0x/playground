namespace DotNetKit.FSharp.UnitTest

open System
open System.Collections
open System.Collections.Generic
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open DotNetKit.FSharp

module ComputationExpressionTest =
  let ``test simple cases`` =
    test {
      do!
        imperative {
          return 0
        }
        |> assertEquals 0
      do!
        imperative {
          let x = 1
          return x + 2
        }
        |> assertEquals 3
      do!
        let mutable sum = 0
        let result =
          imperative {
            sum <- sum + 1
            sum <- sum + 2
            sum <- sum + 3
            return sum
          }
        (sum, result) |> assertEquals (6, 6)
    }

  let ``test if`` =
    let body condition =
      test {
        do!
          imperative {
            if condition then
              return true
            else
              return false
          }
          |> assertEquals condition
      }
    parameterize {
      case true
      case false
      run body
    }

  let ``test zero`` =
    test {
      do!
        let mutable count = 0
        let result =
          imperative {
            count <- count + 1
          }
        (count, result) |> assertEquals (1, ())
    }

  let ``test zero continues computation`` =
    test {
      let mutable count = 0
      do!
        imperative {
          if false then
            exn() |> raise
          count <- count + 1
        }
        |> assertEquals ()
      do! count |> assertEquals 1
    }

  let ``test return stops computation`` =
    test {
      do!
        imperative {
          return 0
          exn() |> raise
          return 1
        }
        |> assertEquals 0
      do!
        imperative {
          if true then
            return 1
          exn() |> raise
          return 2
        }
        |> assertEquals 1
      do!
        imperative {
          while true do
            return 0
        }
        |> assertEquals 0
      do!
        let n = 3
        let mutable count = 0
        let result =
          imperative {
            while true do
              count <- count + 1
              if count = n then
                return count
          }
        (count, result) |> assertEquals (n, n)
    }

  let ``test try-with`` =
    test {
      do!
        imperative {
          try
            return 0
          with
          | _ ->
            exn() |> raise
        }
        |> assertEquals 0
      do!
        let mutable count = 0
        let result =
          imperative {
            try
              ArgumentOutOfRangeException() |> raise
              exn() |> raise
            with
            | :? ArgumentNullException ->
              exn() |> raise
            | :? ArgumentException ->
              count <- count + 1
              return true
            | _ ->
              exn() |> raise
          }
        (count, result) |> assertEquals (1, true)
      do!
        let condition =
          try
            imperative {
              try
                ArgumentException() |> raise
              with
              | :? ArgumentNullException ->
                return false
            }
          with
          | _ ->
            true
        condition |> assertEquals true
    }

  let ``test try-finally`` =
    test {
      do!
        let mutable count = 0
        let result =
          imperative {
            try
              count <- count + 1
              return count
            finally
              count <- count + 1
          }
        (count, result) |> assertEquals (2, 1)
      do!
        let mutable count = 0
        let result =
          try
            imperative {
              try
                count <- count + 1
                exn() |> raise
              finally
                count <- count + 1
            }
            false
          with
          | _ ->
            true
        (count, result) |> assertEquals (2, true)
    }

  let ``test use`` =
    test {
      let get count =
        { new IDisposable with
            override this.Dispose() =
              count |> incr
        }
      do!
        let count = ref 0
        let resource = get count
        let result =
          imperative {
            use r = resource
            return r.Equals(resource)
          }
        (!count, result) |> assertEquals (1, true)
      do!
        let count = ref 0
        let resource = get count
        try
          imperative {
            use r = resource
            exn() |> raise
          }
        with _ -> ()
        !count |> assertEquals 1
    }

  let ``test while`` =
    test {
      let mutable count = 0
      let n = 3
      do!
        imperative {
          while count < n do
            count <- count + 1
        }
        |> assertEquals ()
      do! count |> assertEquals n
    }

  let ``test for`` =
    let body (f, expected, expectedAdvancedCount) =
      test {
        let values = ResizeArray()
        let mutable advancedCount = 0
        let mutable disposedCount = 0
        let n = 5
        let getEnumerator () =
          { new IEnumerator<int> with
              override this.Current =
                advancedCount - 1
              override this.Current =
                advancedCount - 1 :> obj
              override this.MoveNext() =
                advancedCount <- advancedCount + 1
                this.Current < n
              override this.Dispose() =
                disposedCount <- disposedCount + 1
              override this.Reset() =
                NotSupportedException() |> raise
          }
        let iota =
          { new IEnumerable<int> with
              override this.GetEnumerator() = getEnumerator ()
              override this.GetEnumerator() = getEnumerator () :> IEnumerator
          }
        let () =
          try
            f iota values.Add
          with
          | _ -> ()
        do! values |> Seq.toList |> assertEquals expected
        do! advancedCount |> assertEquals expectedAdvancedCount
        do! disposedCount |> assertEquals 1
      }
    parameterize {
      case
        ( fun iota add ->
            imperative {
              for i in iota do
                add i
            }
        , [0; 1; 2; 3; 4]
        , 6
        )
      case
        ( fun iota add ->
            imperative {
              for i in iota do
                add i
                if i = 2 then
                  return ()
            }
        , [0; 1; 2]
        , 3
        )
      case
        ( fun iota add ->
            imperative {
              for i in iota do
                add i
                if i = 2 then
                  exn() |> raise
            }
        , [0; 1; 2]
        , 3
        )
      run body
    }
