namespace DotNetKit.UnitTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open DotNetKit.FSharp

module OptionTest =
  module test_option =
    let ``test return`` =
      test {
        do!
          option {
            return 0
          }
          |> assertEquals (Some 0)
        do!
          option {
            ()
          }
          |> assertEquals None
      }

    let ``test return!`` =
      test {
        do!
          option {
            return! Some 0
          }
          |> assertEquals (Some 0)
        do!
          option {
            return! None
          }
          |> assertEquals None
      }

    let ``test let!`` =
      test {
        do!
          option {
            let! x = Some 1
            return x + 2
          }
          |> assertEquals (Some 3)
        do!
          option {
            let! x = None
            exn() |> raise
            return 0
          }
          |> assertEquals None
      }
