namespace Dyxi.Util.Test

open System
open System.Linq
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.Dried

[<AutoOpen>]
module Misc =
  let tryHalf x =
    if x % 2 = 0 then Some (x / 2) else None

  let map = Map.ofSeq

[<AutoOpen>]
module AssertionExtention =
  let equalsSeq (expected: seq<'a>) (actual: seq<'a>) =
    if Enumerable.SequenceEqual(actual, expected) then pass ()
    else fail (sprintf "Expect: %A\nActual: %A" expected actual)

  let testEqual f (x, expected) =
    test {
      do! (f x) |> assertEquals expected
    }
