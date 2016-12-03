namespace DotNetKit.FSharp

[<AutoOpen>]
module Operators =
  let tap f x =
    f x
    x
