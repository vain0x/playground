namespace Dyxi.Util

[<AutoOpen>]
module Misc =
  let konst x _ = x

  let tap f x = f x; x

  let flip f x y = f y x

  let fold' (xs: #seq<'x>) (f: 'x -> 's -> 's) (s: 's): 's =
    xs |> Seq.fold (flip f) s

  let inline ignore'<'x> (_ : 'x): unit = ()

  let inline assert' (pred: 'x -> bool): 'x -> 'x =
    tap (fun x -> assert (pred x))

  let replace src dst self =
    if self = src then dst else self

  let swap (x: byref<_>) (y: byref<_>): unit =
    let t = x
    do x <- y
    do y <- t
