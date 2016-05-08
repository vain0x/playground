namespace Dyxi.Util.Test

open System
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Dyxi.Util

module SeqTest =
  type Collection<'t>(item: int -> 't, count: int) =
    member this.get_Item(i) = item i
    member this.Count = count

  let ofCollectionTest =
    test {
      let expected  = seq [0; 1; 2]
      let col       = Collection(id, 3)
      let actual    = col |> Seq.ofCollection
      do! actual |> equalsSeq expected
    }

  let tryUnconsTest =
    let tryUncons (xs: list<'x>) =
      xs |> Seq.tryUncons |> Option.map (fun (h, t) -> (h, t |> Seq.toList))
    parameterize {
      case ([], None)
      case ([0; 1; 2], Some (0, [1; 2]))
      run (testEqual tryUncons)
    }

  let zipShrinkTest =
    let zipShrink (l, r) =
      Seq.zipShrink l r |> Seq.toList
    parameterize {
      case (([1; 2], seq ['a'; 'b'; 'c']), [(1, 'a'); (2, 'b')])
      run (testEqual zipShrink)
    }

module OptionTest =
  let ofPairTest =
    parameterize {
      case (Int32.TryParse("x"), None)
      case (Int32.TryParse("1"), Some 1)
      run (testEqual Option.ofPair)
    }

module ListTest =
  let tryMaxByTest =
    parameterize {
      case ([], None)
      case ([1; 2; 3], Some 3)
      run (testEqual (List.tryMaxBy id))
    }

  let uniqueByTest =
    parameterize {
      case ([], [])
      case ([1; 2; 3], [1; 2; 3])
      case ([11; 2; 1; 3], [11; 2; 3])
      run (testEqual (List.uniqueBy (fun x -> x % 10)))
    }

  let partitionMapTest =
    parameterize {
      case ([5; 6; 7; 8], ([3; 4], [5; 7]))
      run (testEqual (List.partitionMap tryHalf))
    }

  let tailpadTest =
    parameterize {
      case ([0; 1], [0; 1; 9; 9])
      case ([0; 1; 2; 3; 4; 5], [0; 1; 2; 3; 4; 5])
      run (testEqual (List.tailpad 4 9))
    }

module SetTest =
  let ofOptionTest =
    parameterize {
      case (Some 0, Set.singleton 0)
      case (None, Set.empty)
      run (testEqual Set.ofOption)
    }

  let collectTest =
    test {
      let f x = set [x; x * 2]
      do! (set [2; 3; 4] |> Set.collect f) |> assertEquals (set [2; 3; 4; 6; 8])
    }

  let chooseTest =
    test {
      do! (set [Some 0; Some 1; None] |> Set.choose id) |> assertEquals (set [0; 1])
    }

  let tryFind =
    parameterize {
      case (set [0], None)
      case (set [1], Some 1)
      run (testEqual (Set.tryFind 1))
    }

module MapTest =
  let m3 =
    map [(0, "a"); (1, "b"); (2, "c")]

  let m4 =
    map [(0, "x"); (1, "x"); (2, "y"); (3, "z")]

  let keysTest =
    test {
      do! (m3 |> Map.keys |> Seq.toList) |> assertEquals [0; 1; 2]
    }

  let valuesTest =
    test {
      do! (m3 |> Map.values |> Seq.toList) |> assertEquals ["a"; "b"; "c"]
    }

  let sizeTest =
    parameterize {
      case (Map.empty, 0)
      case (m3, 3)
      run (testEqual Map.size)
    }

  let pullBackTest =
    let pb (m, y) = m |> Map.pullBack y |> Seq.toList
    parameterize {
      case ((m4, "x"), [0; 1])
      case ((m4, "y"), [2])
      case ((m4, "w"), [])
      run (testEqual pb)
    }

  let updateTest =
    let f (m, k, v) = m |> Map.update k v
    parameterize {
      case ((m3, 0, Some "x" ), ((m3 |> Map.add 0 "x"   ), Some "a"))
      case ((m3, 0, None     ), ((m3 |> Map.remove 0    ), Some "a"))
      case ((m3, 9, Some "x" ), ((m3 |> Map.add 9 "x"   ), None))
      run (testEqual f)
    }

  let chooseTest =
    parameterize {
      case (map [(0, Some "a"); (1, Some "b"); (2, Some "c"); (3, None)], m3)
      run (testEqual (Map.choose (fun k v -> v)))
    }

  let appendTest =
    parameterize {
      case ((m3, m4), m4)
      case ((m4, m3), m3 |> Map.add 3 "z")
      run (testEqual (fun (l, r) -> Map.append l r))
    }

  let appendWithTest =
    parameterize {
      case ((m3, m4), map [(0, "ax"); (1, "bx"); (2, "cy"); (3, "z")])
      case ((m4, m3), map [(0, "xa"); (1, "xb"); (2, "yc"); (3, "z")])
      run (testEqual (fun (l, r) -> Map.appendWith (+) l r))
    }

//module Dictionary =
//module Observable =
