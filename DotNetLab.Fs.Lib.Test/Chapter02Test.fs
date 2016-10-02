namespace DotNetLab.Fs.Lib.PFDS

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open DotNetLab.Fs.Lib.PFDS.Chapter02

module Chapter02Test =
  let suffixTest =
    test {
      let actual = [1; 2; 3; 4] |> suffix
      let expected =
        [
          [1; 2; 3; 4]
          [2; 3; 4]
          [3; 4]
          [4]
          []
        ]
      do! actual |> assertEquals expected
    }

  let testISet (s: ISet<int, BinarySearchTree<int>>) =
    [
      test {
        let empty = s.Empty
        do! empty.Contains(0) |> assertEquals false
      }
      test {
        let empty = s.Empty
        let set = empty.Insert(0).Insert(1)
        do! set.Contains(0) |> assertEquals true
        do! set.Contains(1) |> assertEquals true
        do! set.Contains(2) |> assertEquals false
      }
      test {
        let set = s.Empty.Insert(0).Insert(0)
        do! set.Contains(0) |> assertEquals true
      }
    ]

  let testBinarySearchTree =
    testISet Empty
