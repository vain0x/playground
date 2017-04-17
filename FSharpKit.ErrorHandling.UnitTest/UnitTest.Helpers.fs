namespace FSharpKit.UnitTest

open Persimmon

[<AutoOpen>]
module Helpers =
  let test = UseTestNameByReflection.test
  let parameterize = parameterize
  let trap = trap
  let is = assertEquals
