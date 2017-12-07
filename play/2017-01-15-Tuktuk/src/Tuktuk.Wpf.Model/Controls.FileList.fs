namespace Tuktuk.Wpf.Controls

open System
open System.Collections.Generic
open System.IO
open System.Reactive.Concurrency
open System.Reactive.Linq
open DotNetKit.Functional.Commands
open Reactive.Bindings
open SharpFileSystem
open SharpFileSystem.FileSystems
open Tuktuk.Reactive.Bindings

[<Sealed>]
type FileListItem
  ( fileSystem: IFileSystem
  , path: FileSystemPath
  , launchCommand: ReactiveCommand<FileSystemPath>
  ) =
  let lastUpdateDateTime =
    DateTime.Now |> ReactiveProperty.create

  let launchCommand =
    UnitCommand(fun () -> launchCommand.Execute(path))

  member this.Name =
    path.EntityName

  member this.LaunchCommand =
    launchCommand

  member this.LastUpdateDateTime =
    lastUpdateDateTime

[<Sealed>]
type FileList
  ( fileSystem: IFileSystem
  , directoryPath: FileSystemPath
  , items: IReadOnlyList<FileListItem>
  , launchCommand: ReactiveCommand<FileSystemPath>
  ) =
  let items = items |> ReactiveCollection.ofSeq

  static member private Fetch(fileSystem, directoryPath, launchCommand) =
    let subpaths =
      try
        (fileSystem: IFileSystem).GetEntities(directoryPath) |> Seq.toArray
      with
      | _ -> Array.empty
    let items =
      subpaths
      |> Array.sortBy (fun path -> path.EntityName)
      |> Array.sortBy (fun path -> if path.IsDirectory then 0 else 1)
      |> Array.map (fun path -> FileListItem(fileSystem, path, launchCommand))
    items :> IReadOnlyList<_>

  new(fileSystem, directoryPath, launchCommand) =
    let items = FileList.Fetch(fileSystem, directoryPath, launchCommand)
    new FileList(fileSystem, directoryPath, items, launchCommand)

  member this.Items =
    items

  member this.Dispose() =
    items.Dispose()

  interface IDisposable with
    override this.Dispose() =
      this.Dispose()
