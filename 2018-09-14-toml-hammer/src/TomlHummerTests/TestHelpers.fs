[<AutoOpen>]
module XunitHelpers

open Xunit

let inline is<'T> (expected: 'T) (actual: 'T): unit =
  Assert.Equal(expected, actual)
