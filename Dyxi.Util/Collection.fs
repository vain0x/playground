[<AutoOpen>]
module Dyxi.Util.Collections

module Seq =
  let inline ofCollection
      (self: ^t when ^t: (member Item: int -> _) and ^t: (member Count: int))
      =
    let len = (^t: (member Count: int) self)
    seq {
      for i in 0..(len - 1) ->
        (^t: (member Item: int -> _) (self, i))
      }

module Option =
  let ofPair: bool * 't -> option<'t> =
    function
    | (true, value) -> Some value
    | _ -> None

module List =
  /// Apply f for each element in xs and partition them into two list.
  /// The fst is y's where f x = Some y
  /// and the other is x's where f x = None.
  let paritionMap (f: 'x -> option<'y>) (xs: list<'x>): (list<'y> * list<'x>) =
    xs
    |> List.fold (fun (l, r) x ->
        match f x with
        | Some y -> (y :: l, r)
        | None -> (l, x :: r)
        ) ([], [])
    |> (fun (l, r) -> (l |> List.rev, r |> List.rev))

module Map =
  let singleton (k: 'k) (v: 'v): Map<'k, 'v> =
    Map.ofList [(k, v)]

  let append (l: Map<'k, 'v>) (r: Map<'k, 'v>): Map<'k, 'v> =
    r |> Map.fold (fun l k v -> l |> Map.add k v) l
    
  let keys (m: Map<'k, 'v>): seq<'k> =
    m |> Map.toSeq |> Seq.map fst

  let values (m: Map<'k, 'v>): seq<'v> =
    m |> Map.toSeq |> Seq.map snd

  let pullBack (value: 'v) (m: Map<'k, 'v>): seq<'k> =
    m
    |> Map.toSeq
    |> Seq.choose (fun (k, v) ->
        if v = value then Some k else None
        )

  let choose (f: 'k -> 'v -> option<'w>) (m: Map<'k, 'v>): Map<'k, 'w> =
    m |> Map.fold (fun m k v ->
        match f k v with
        | None      -> m
        | Some v'   -> m |> Map.add k v'
        ) Map.empty

  /// The number of key-value pairs
  let size self: int =
    self |> Map.toSeq |> Seq.length
