namespace Tuktuk.Wpf.Controls

open System
open System.Collections.Generic
open System.IO
open System.Reactive.Concurrency
open System.Reactive.Linq
open Reactive.Bindings
open SharpFileSystem
open SharpFileSystem.FileSystems
open Tuktuk.Reactive.Bindings

[<Sealed>]
type FileCollectionItem
  ( fileSystem: IFileSystem
  , path: FileSystemPath
  ) =
  member this.Name =
    path.EntityName

[<Sealed>]
type FileCollection
  ( fileSystem: IFileSystem
  , directoryPath: FileSystemPath
  , items: IReadOnlyList<FileCollectionItem>
  ) =
  let items = items |> ReactiveCollection.ofSeq

  static member Fetch(fileSystem, directoryPath) =
    let subpaths =
      try
        (fileSystem: IFileSystem).GetEntities(directoryPath) |> Seq.toArray
      with
      | _ -> Array.empty
    let items =
      subpaths
      |> Array.sortBy (fun path -> path.EntityName)
      |> Array.sortBy (fun path -> if path.IsDirectory then 0 else 1)
      |> Array.map (fun path -> FileCollectionItem(fileSystem, path))
    items :> IReadOnlyList<_>

  new(fileSystem, directoryPath) =
    let items = FileCollection.Fetch(fileSystem, directoryPath)
    new FileCollection(fileSystem, directoryPath, items)

  member this.Items =
    items

  member this.Dispose() =
    items.Dispose()

  interface IDisposable with
    override this.Dispose() =
      this.Dispose()
