namespace Util.Collections

open System.Collections.Generic

[<AutoOpen>]
module Misc =
  let fst'3 (t0, t1, t2) = t0
  let snd'3 (t0, t1, t2) = t1
  let thr'3 (t0, t1, t2) = t2

[<RequireQualifiedAccess>]
module Option =
  let union' (proc: unit -> _ option) = function
    | Some x -> Some x
    | None -> proc ()

  let intersect' proc = function
    | Some x -> proc ()
    | None -> None

  /// operator || in javascript; ordered, not shortcircuit
  /// mplus in MonadPlus; or (<|>) in Alternative
  let union opt2 = union' (fun () -> opt2)

  /// operator && in javascript; ordered, not shortcircuit
  let intersect opt2 = intersect' (fun () -> opt2)

  /// from bool to option
  let if' (b: bool) (proc: unit -> _) =
    if b then Some (proc ()) else None

  /// wrap in option with predcate
  let ifSat (pred: 'a -> bool) (x: 'a) =
    if pred x then Some x else None

  /// wrap a nullable non-null value in Some if non-null, otherwise None
  let ofNullable (x : 'a when 'a : null) = 
    x |> ifSat ((<>) null)

  /// (success, value) -> Some value or None
  let trialResult (b: bool, value) =
    if b then Some value else None

  /// seq.filter
  let filter pred =
    Option.bind (ifSat pred)

  let toSeq self =
    seq {
      match self with
      | Some x -> yield x
      | None -> ()
    }

[<RequireQualifiedAccess>]
module Seq =
  let iota n = seq { 0..(n - 1) }

  let private mapIfNonEmpty f self =
    Option.if' (self |> Seq.isEmpty |> not) (fun () -> self |> f)

  let tryFirst self = self |> mapIfNonEmpty Seq.head
  let tryLast  self = self |> mapIfNonEmpty Seq.last
  let tryMax   self = self |> mapIfNonEmpty Seq.max
  let tryMin   self = self |> mapIfNonEmpty Seq.min

  /// 添字つき (自然数列との zip)
  let indexed self =
    self |> Seq.zip (self |> Seq.length |> iota)

  let contains value =
    Seq.exists ((=) value)

  /// n 要素回転させる 
  let rotate n self =
    if self |> Seq.isEmpty || n = 0
    then self
    else
      let len = self |> Seq.length
      let n =  ((n + len) % len) % len
      Seq.append (self |> Seq.skip n) (self |> Seq.take n)

[<RequireQualifiedAccess>]
module List =
  let cons h t = h :: t

  let maxWithIndex f =
    Seq.indexed >> List.ofSeq >> (List.maxBy f)

  let tryAssoc key self =
    let f l (k, v) =
        let r = Option.if' (k = key) (fun () -> v)
        (l, r) ||> Option.union
    self |> List.fold f None

  let tryAssocUnzip key (keys, vals) =
    keys
    |> List.tryFindIndex ((=) key)
    |> Option.map (fun i -> vals |> Seq.nth i)

[<RequireQualifiedAccess>]
module Array =
  // from collection object with .Count and .Item
  let inline ofCol< ^T, ^U when ^T: (member Count: int) and ^T: (member Item: int -> ^U)> (o: ^T): ^U[] =
    [| for i in 0..((^T: (member Count: int) o) - 1)
        -> (^T: (member Item: int -> ^U) (o, i)) |]

  let take n a = Array.sub a 0 n
  let drop n a = Array.sub a 1 (Array.length a - 1)

[<RequireQualifiedAccess>]
module Ref =
  let apply f r = (r := (f !r))
  let inc = apply ((+) 1)
  let dec = apply ((-) 1)

[<RequireQualifiedAccess>]
module Map =
  let toAssocList self =
    self |> Map.toList |> List.unzip

  let keys   self = self |> toAssocList |> fst
  let values self = self |> toAssocList |> snd

  /// find all keys whose value equals to the given value
  let pullBack value map =
    map
    |> Map.toSeq
    |> Seq.choose (fun (k, v) -> Option.if' (v = value) (fun () -> k))

  let toDictionary (self : Map<'Key, 'Value>) =
    let dict = new Dictionary<'Key, 'Value>()
    for KeyValue kv in self do
      dict.Add kv
    dict
