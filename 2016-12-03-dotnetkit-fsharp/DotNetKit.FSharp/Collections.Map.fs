namespace DotNetKit.FSharp

[<RequireQualifiedAccess>]
module Map =
  let singleton (key: 'k) (value: 'v): Map<'k, 'v> =
    Map.empty |> Map.add key value

  let append (l: Map<'k, 'v>) (r: Map<'k, 'v>): Map<'k, 'v> =
    r |> Map.fold (fun l k v -> l |> Map.add k v) l

  let keys (map: Map<'k, 'v>): seq<'k> =
    map |> Map.toSeq |> Seq.map fst

  let values (map: Map<'k, 'v>): seq<'v> =
    map |> Map.toSeq |> Seq.map snd

  let pullBack (value: 'v) (map: Map<'k, 'v>): seq<'k> =
    map
    |> Map.toSeq
    |> Seq.choose
      (fun (k, v) -> if v = value then Some k else None)

  let choose (f: 'k -> 'v -> option<'w>) (map: Map<'k, 'v>): Map<'k, 'w> =
    let update map k v =
      match f k v with
      | Some w ->
        map |> Map.add k w
      | None ->
        map
    map |> Map.fold update Map.empty

  let size (map: Map<_, _>): int =
    map.Count

  let findOrAdd (key: 'k) (getValue: unit -> 'v) (map: Map<'k, 'v>) =
    match map |> Map.tryFind key with
    | Some v ->
      (map, v)
    | None ->
      let v = getValue ()
      (map |> Map.add key v, v)
