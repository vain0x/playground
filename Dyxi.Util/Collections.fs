namespace Dyxi.Util

open System
open System.Collections.Generic

module Seq =
  let inline ofCollection
    (self: ^t when ^t: (member Item: int -> _ with get) and ^t: (member Count: int))
    =
    let len = (^t: (member Count: int) self)
    in seq { for i in 0..(len - 1) -> (^t: (member Item: int -> _ with get) (self, i)) }

  let tryUncons (xs: seq<'x>): option<'x * seq<'x>> =
    if xs |> Seq.isEmpty
    then None
    else Some (xs |> Seq.head, xs |> Seq.tail)

  let zipShrink  (l: seq<'l>) (r: seq<'r>): seq<'l * 'r> =
    let unfolder (l, r) =
      match (l |> tryUncons, r |> tryUncons) with
      | (Some (lh, lt), Some (rh, rt)) ->
          Some ((lh, rh), (lt, rt))
      | _ -> None
    in Seq.unfold unfolder (l, r)

module Option =
  let ofPair =
    function
    | (true, value) -> Some value
    | _ -> None

module List =
  let tryMaxBy (proj: 'x -> 'y) (xs: list<'x>): option<'x> =
    let folder ma x =
      let projX = proj x
      let ma' =
        match ma with
        | Some (_, projMax) when projMax >= projX -> ma
        | _ -> Some (x, projX)
      in ma'
    in xs |> List.fold folder None |> Option.map fst

  let uniqueBy (f: 'x -> 'y) (xs: list<'x>) =
    let folder (acc, set) x =
      let y = f x
      if set |> Set.contains y
      then (acc, set)
      else (x :: acc, set |> Set.add y)
    in xs |> List.fold folder ([], Set.empty) |> fst |> List.rev

  /// Apply each element in xs to f and partition them into two list.
  /// The fst is y's where f x = Some y
  /// and the other is x's where f x = None.
  let partitionMap (f: 'x -> option<'y>) (xs: list<'x>): (list<'y> * list<'x>) =
    let folder (l, r) x =
      match f x with
      | Some y -> (y :: l, r)
      | None -> (l, x :: r)
    let (l, r) =
      xs |> List.fold folder ([], [])
    in (l |> List.rev, r |> List.rev)

  /// Add x's to back so that the list has at least n elements.
  let tailpad n x self =
    let len     = self |> List.length
    let count   = max 0 (n - len)
    in self @ (List.replicate count x)

module Set =
  let ofOption: option<'x> -> Set<'x> =
    function
    | Some x        -> Set.singleton x
    | None          -> Set.empty

  let collect (f: 'x -> Set<'y>) (self: Set<'x>): Set<'y> =
    self |> Seq.map f |> Set.unionMany

  let choose (f: 'x -> option<'y>) (self: Set<'x>): Set<'y> =
    self |> collect (fun x -> f x |> ofOption)

  let tryFind (x: 'x) (self: Set<'x>): option<'x> =
    if self |> Set.contains x then Some x else None

module Map =
  let singleton (k: 'k) (v: 'v): Map<'k, 'v> =
    Map.ofList [(k, v)]

  let keys (m: Map<'k, 'v>): seq<'k> =
    m |> Map.toSeq |> Seq.map fst

  let values (m: Map<'k, 'v>): seq<'v> =
    m |> Map.toSeq |> Seq.map snd

  /// The number of key-value pairs
  let size self: int =
    self |> Map.toSeq |> Seq.length

  let pullBack (value: 'v) (m: Map<'k, 'v>): seq<'k> =
    m |> Seq.choose (fun (KeyValue (k, v)) ->
      if v = value then Some k else None
      )

  let update (key: 'k) (valueOpt: option<'v>) (self: Map<'k, 'v>): (Map<'k, 'v> * option<'v>) =
    let old     = self |> Map.tryFind key
    let self'   =
      match valueOpt with
      | Some value  -> self |> Map.add key value
      | None        -> self |> Map.remove key
    in (self', old)

  let choose (f: 'k -> 'v -> option<'w>) (m: Map<'k, 'v>): Map<'k, 'w> =
    let folder m k v =
      match f k v with
      | None      -> m
      | Some v'   -> m |> Map.add k v'
    in m |> Map.fold folder Map.empty

  let append (l: Map<'k, 'v>) (r: Map<'k, 'v>): Map<'k, 'v> =
    r |> Map.fold (fun l k v -> l |> Map.add k v) l

  let appendWith (f: 'x -> 'x -> 'x) (l: Map<'k, 'x>) (r: Map<'k, 'x>): Map<'k, 'x> =
    let folder f m k v =
      let v' =
        match m |> Map.tryFind k with
        | None      -> v
        | Some v'   -> f v v'
      in m |> Map.add k v'
    let body f l r =
      l |> Map.fold (folder f) r
    if (l |> size) < (r |> size)
    then body f l r
    else body (flip f) r l

module Dictionary =
  let toSeq (self: Dictionary<'k, 'v>): seq<'k * 'v> =
    (self :> seq<KeyValuePair<'k, 'v>>)
    |> Seq.map (fun (KeyValue (k, v)) -> (k, v))

  let ofSeq (kvs: seq<'k * 'v>): Dictionary<'k, 'v> =
    Dictionary() |> tap (fun dict ->
      kvs |> Seq.iter (fun (k, v) -> dict.Add(k, v) |> ignore)
      )

  let toMap (self: Dictionary<'k, 'v>): Map<'k, 'v> =
    self |> toSeq |> Map.ofSeq

  let ofMap (m: Map<'k, 'v>): Dictionary<'k, 'v> =
    m |> Map.toSeq |> ofSeq
