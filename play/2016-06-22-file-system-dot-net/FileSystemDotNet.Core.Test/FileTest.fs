namespace FileSystemDotNet.Core.Test

open System.IO
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open FileSystemDotNet.Core

module FileSystemTest =
  open TestData

  let fullNameTest =
    test {
      let file = FileSystem.d /. "todo.txt"
      do! (file |> File.fullName) |> assertEquals (Path.Combine("D", "todo.txt"))
    }
