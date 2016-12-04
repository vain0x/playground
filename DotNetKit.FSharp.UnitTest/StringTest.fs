namespace DotNetKit.FSharp.UnitTest

open System
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open DotNetKit.FSharp

module StringTest =
  let ``test toSeq`` =
    parameterize {
      case ("", [])
      case ("hello", ['h'; 'e'; 'l'; 'l'; 'o'])
      run
        (fun (it, expected) ->
          test {
            do! it |> String.toSeq |> Seq.toList |> assertEquals expected
          }
        )
    }

  let ``test tryFindIndex`` =
    parameterize {
      case ("hello", Some 1)
      case ("world", None)
      run
        (fun (it, expected) ->
          test {
            do! it |> String.tryFindIndex ((=) 'e') |> assertEquals expected
          }
        )
    }

  let ``test takeWhile`` =
    parameterize {
      case ("", "")
      case (" ", "")
      case ("hello!", "hello!")
      case ("hello world", "hello")
      run
        (fun (it, expected) ->
          test {
            do! it |> String.takeWhile (Char.IsWhiteSpace >> not) |> assertEquals expected
          }
        )
    }

  let ``test truncate`` =
    parameterize {
      case ("", "")
      case ("012", "012")
      case ("012345", "012")
      run
        (fun (it, expected) ->
          test {
            do! it |> String.truncate 3 |> assertEquals expected
          }
        )
    }

  let ``test splitAt`` =
    parameterize {
      case ("", 0, Some ("", ""))
      case ("a", 0, Some ("", "a"))
      case ("a", 1, Some ("a", ""))
      case ("ab", 1, Some ("a", "b"))
      case ("", -1, None)
      case ("", 1, None)
      run
        ( fun (source, index, expected) ->
            test {
              let it = source |> Option.tryApply (String.splitAt index)
              do! it |> assertEquals expected
            }
        )
    }
