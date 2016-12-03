namespace DotNetKit.FSharp

open System.Collections.Generic

[<RequireQualifiedAccess>]
module Dictionary =
  let tryFind key (this: Dictionary<_, _>) =
    match this.TryGetValue(key) with
    | (true, value) ->
      Some value
    | (false, _) ->
      None

  let ofSeq kvs =
    let this = Dictionary()
    for (key, value) in kvs do
      this.Add(key, value)
    this
