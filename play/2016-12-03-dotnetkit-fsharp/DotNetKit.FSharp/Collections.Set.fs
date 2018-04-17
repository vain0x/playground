namespace DotNetKit.FSharp

[<RequireQualifiedAccess>]
module Set =
  let ofOption (x: option<'x>): Set<'x> =
    x |> Option.toArray |> Set.ofArray

  let collect (f: 'x -> Set<'y>) (this: Set<'x>): Set<'y> =
    this |> Seq.map f |> Set.unionMany

  let choose (f: 'x -> option<'y>) (this: Set<'x>): Set<'y> =
    this |> collect (f >> ofOption)
