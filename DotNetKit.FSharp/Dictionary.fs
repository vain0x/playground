namespace DotNetKit.FSharp

open System.Collections.Generic

[<RequireQualifiedAccess>]
module Dictionary =
  let ofSeq kvs =
    let this = Dictionary()
    for (key, value) in kvs do
      this.Add(key, value)
    this
