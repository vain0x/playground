namespace FileSystemDotNet.Core.Test

open System.IO
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open FileSystemDotNet.Core

module DirectoryTest =
  open TestData

  let ancestorsTest =
    let f (dir, expected) =
      test {
        let path =
          dir |> Directory.ancestors
          |> List.map (fun dir -> dir.Name)
          |> List.toArray
          |> Path.Combine
        do! path |> assertEquals expected
      }
    in
      parameterize {
        case (FileSystem.d ./ "repo" ./ "tzix", Path.Combine("D", "repo"))
        run f
      }
