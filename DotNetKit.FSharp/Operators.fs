namespace DotNetKit.FSharp

[<AutoOpen>]
module Operators =
  let tap f x =
    f x
    x

  let flip f x y =
    f y x
