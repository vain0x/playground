namespace DotNetKit.FSharp

open System.Collections.Generic

[<RequireQualifiedAccess>]
module Dictionary =
  let tryFind (key: 'k) (this: Dictionary<'k, 'v>): option<'v> =
    match this.TryGetValue(key) with
    | (true, value) ->
      Some value
    | (false, _) ->
      None

  let ofSeq (kvs: seq<'k * 'v>): Dictionary<'k, 'v> =
    let this = Dictionary()
    for (key, value) in kvs do
      this.Add(key, value)
    this
