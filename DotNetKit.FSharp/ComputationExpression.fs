namespace DotNetKit.FSharp.ComputationExpression

open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
module Default =
  let tryWith (f: unit -> 'x) (h: exn -> 'x): 'x =
    try
      f ()
    with
    | e ->
      h e

  let tryFinally (f: unit -> 'x) (g: unit -> unit): 'x =
    try
      f ()
    finally
      g ()

  let ``while`` combine zero (p: unit -> bool) (f: unit -> _) =
    let rec loop () =
      if p () then
        let x = f ()
        combine (x, loop)
      else
        zero ()
    loop ()

  let ``for`` using ``while`` (xs: #seq<'x>) f =
    using
      ( xs.GetEnumerator()
      , (fun e ->
          ``while`` ((e :> IEnumerator<_>).MoveNext, (fun () -> f e.Current))
        )
      )
