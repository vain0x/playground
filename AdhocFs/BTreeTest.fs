namespace AdhocFs

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection

module BTreeTest =
  let kvs =
    [4; 6; 5; 1; 3; 2]
    |> List.mapi (fun i x -> (x, i))

  let btree () =
    let b = BTree.empty
    for (k, v) in kvs do
      b |> BTree.add k v
    b

  let emptyTest =
    test {
      let b = BTree.empty
      do! b |> BTree.length |> assertEquals 0
    }

  let insertTest =
    test {
      let b = btree ()
      do! b |> BTree.length |> assertEquals (kvs |> List.length)
    }

  let findSuccessTest =
    let b = btree ()
    let f (k, v) =
      test {
        do! b |> BTree.tryFind k |> assertEquals (Some v)
      }
    parameterize {
      case (4, 0)
      case (6, 1)
      case (5, 2)
      case (1, 3)
      case (3, 4)
      case (2, 5)
      run f
    }

  let findFailureTest =
    let b = btree ()
    let f k =
      test {
        do! b |> BTree.tryFind k |> assertEquals None
      }
    parameterize {
      case 0
      case (-1)
      case 7
      run f
    }

  let deleteTest =
    let f k =
      test {
        let b = btree ()
        b |> BTree.remove k
        do! b |> BTree.tryFind k |> assertEquals None
      }
    parameterize {
      case 0
      case 1
      case 2
      run f
    }

  let toListTest =
    test {
      do! (btree () |> BTree.toList |> Map.ofList) |> assertEquals (kvs |> Map.ofList) 
    }
