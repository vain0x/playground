namespace Tuktuk.Wpf

open System
open System.Reactive.Disposables
open System.Reactive.Linq
open DotNetKit.FSharp
open Reactive.Bindings
open Reactive.Bindings.Extensions
open SharpFileSystem
open SharpFileSystem.FileSystems
open Tuktuk.Reactive.Bindings
open Tuktuk.Wpf.Controls

[<Sealed>]
type Main(shelve) =
  static member CreateShelve() =
    let fileSystem = PhysicalFileSystem.SuperRoot
    let drives = fileSystem.GetEntities(FileSystemPath.Root)
    let books =
      drives |> Seq.map
        (fun drive ->
          let currentDirectory =
            fileSystem.GetVirtualDirectoryPath(Environment.CurrentDirectory)
          let paths =
            [| drive; currentDirectory |]
          let pages =
            paths |> Array.map (fun path -> new Page(fileSystem, path))
          new Book(fileSystem, sprintf "Book %s" drive.EntityName, pages)
        )
      |> Seq.toArray
    let workspaces =
      [|
        new Workspace(books.[0].ActivePage.Value)
        new Workspace(books.[1].ActivePage.Value)
      |]
    new Shelve(fileSystem, books, workspaces)

  new() =
    let shelve = Main.CreateShelve()
    new Main(shelve)

  member this.Shelve =
    shelve
