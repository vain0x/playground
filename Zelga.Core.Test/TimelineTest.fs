namespace Zelga.Core.Test

open System
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Zelga.Core

module TimelineTest =
  let zero = DateTimeOffset.MinValue

  let empty () =
    Timeline<int>(fun i -> zero.AddDays(float i))

  let ofSeq xs =
    let timeline = empty ()
    for x in xs do
      timeline.Add(x)
    timeline

  let toList timeline =
    timeline |> Seq.toList

  let test_add_in_ascending_order =
    test {
      let timeline = empty ()
      timeline.Add(0)
      timeline.Add(1)
      timeline.Add(2)
      do! timeline |> toList |> assertEquals [0; 1; 2]
    }

  let test_add_in_descending_order =
    test {
      let timeline = empty ()
      timeline.Add(2)
      timeline.Add(1)
      timeline.Add(0)
      do! timeline |> toList |> assertEquals [0; 1; 2]
    }

  let test_add_randomly =
    let random = Random()
    let timeline = empty ()
    let xs = [ for i in 0..100 -> random.Next(0, 100) ]
    for x in xs do
      timeline.Add(x)
    test {
      do! timeline |> toList |> assertEquals (xs |> List.sort)
    }

  let test_it_notifies_adding =
    test {
      let notifyCount = ref 0
      let timeline = empty ()
      use subscription =
        timeline |> Observable.subscribe (fun _ -> notifyCount := !notifyCount + 1)
      timeline.Add(0)
      timeline.Add(1)
      do! !notifyCount |> assertEquals 2
    }
