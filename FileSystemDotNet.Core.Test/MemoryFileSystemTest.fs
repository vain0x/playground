namespace FileSystemDotNet.Core.Test

open System.IO
open FParsec
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open FileSystemDotNet.Core

module TestData =
  let private fsysSource =
    """
      C { user { .gitconfig } }
      D {
        .git {
          config
          HEAD
        }
        repo {
          tzix {
            Tzix.Model {}
            Tzix.View {
              bin { Debug { tzix.exe ~x } }
            }
          }
        }
        todo.txt
      }
    """

  let fsys =
    fsysSource |> MemoryFileSystem.Parser.parse "testData"

  let (./) (dir: IDirectory) (name: string) =
    dir |> Directory.tryFindDirectory name |> Option.get

  let (/.) (dir: IDirectory) (name: string) =
    dir |> Directory.tryFindFile name |> Option.get

  module FileSystem =
    let c = fsys.Roots.[0]
    let d = fsys.Roots.[1]

module MemoryFileSystemTest =
  open TestData

  let parseTest = test {
    do! fsys.Roots |> Array.map (fun dir -> dir.Name) |> assertEquals [|"C"; "D"|]
  }
