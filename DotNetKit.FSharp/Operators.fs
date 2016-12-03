namespace DotNetKit.FSharp

open System

[<AutoOpen>]
module Operators =
  let tap f x =
    f x
    x

  let flip f x y =
    f y x

  let todo message =
    NotImplementedException(message) |> raise
