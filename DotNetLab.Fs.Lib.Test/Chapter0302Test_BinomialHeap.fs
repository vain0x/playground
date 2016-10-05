namespace DotNetLab.Fs.Lib.PFDS

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open DotNetLab.Fs.Lib.PFDS.Chapter0302

module Chapter0302Test =
  open Chapter0301Test

  let testBinomialHeap =
    testHeap BinomialHeap.signature

  module Exercise05 =
    let testBinomialHeap' =
      testHeap (Exercise05.BinomialHeap.signature' ())

  module Exercise06 =
    let testBinomialHeap'' =
      testHeap Exercise06.BinomialHeap.signature

  module Exercise07 =
    let testExplicitMinHeap =
      [
        yield! testHeap (Exercise07.ExplicitMinHeap.signature Chapter0301.LeftistHeap.AsHeap)
        yield! testHeap (Exercise07.ExplicitMinHeap.signature BinomialHeap.signature)
      ]
