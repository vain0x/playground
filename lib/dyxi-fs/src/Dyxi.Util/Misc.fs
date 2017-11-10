namespace Dyxi.Util

[<AutoOpen>]
module Misc =
  let tap (f: 'x -> unit) (x: 'x): 'x = f x; x

  let flip f x y = f y x

  let konst x _ = x

  let fold' (xs: #seq<'x>) (f: 'x -> 's -> 's) (s: 's): 's =
    xs |> Seq.fold (flip f) s 
