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
