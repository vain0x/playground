namespace DotNetKit.FSharp

open System

[<AutoOpen>]
module Operators =
  let tap (f: 'x -> unit) (x: 'x): 'x =
    f x
    x

  let flip (f: 'y -> 'x -> 'z) (x: 'x) (y: 'y): 'z =
    f y x

  let todo (message: string): _ =
    NotImplementedException(message) |> raise

  let tryCast<'x, 'y> (x: 'x): option<'y> =
    match x |> box with
    | :? 'y as y ->
      Some y
    | _ ->
      None

  let fold (xs: #seq<'x>) (f: 'x -> 's -> 's) (s: 's): 's =
    xs |> Seq.fold (flip f) s 
