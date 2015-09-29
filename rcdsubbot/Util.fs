[<AutoOpen>]
module Util

let konst x _ = x
let curry2 f x y = f (x, y)

module Option =
  let getOr' f = function
    | Some x -> x
    | None -> f ()

  let getOr y = function
    | Some x -> x
    | None -> y

module List =
  let assoc key =
    List.tryFind (fun (k, v) -> k = key)
    >> Option.map snd
