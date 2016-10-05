namespace DotNetLab.Fs.Lib.PFDS

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open DotNetLab.Fs.Lib.PFDS.Chapter0302

module Chapter0302Test =
  let testBinomialHeap =
    let empty = BinomialHeap.empty
    let toList h = h |> BinomialHeap.toSeq |> Seq.toList
    [
      yield
        test {
          do! empty |> BinomialHeap.findMin |> assertEquals None
          do! empty |> BinomialHeap.deleteMin |> assertEquals None
        }
      yield 
        test {
          let heap = BinomialHeap.ofSeq [3; 1; 2]
          do! heap |> BinomialHeap.findMin |> assertEquals (Some 1)
        }
      yield
        test {
          let heap = BinomialHeap.ofSeq [5; 2; 1; 2; 3; 4; 5; 2]
          do! heap |> BinomialHeap.findMin' |> assertEquals (Some 1)
        }
      yield
        test {
          let heap = BinomialHeap.ofSeq [3; 1; 2]
          do! heap |> toList |> assertEquals [1; 2; 3]
        }
      yield
        test {
          do! [] |> BinomialHeap.ofSeq |> toList |> assertEquals []
        }
      yield
        test {
          do! [3; 1; 2] |> BinomialHeap.ofSeq |> toList |> assertEquals [1; 2; 3]
        }
    ]
