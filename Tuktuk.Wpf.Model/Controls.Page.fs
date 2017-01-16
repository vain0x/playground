namespace Tuktuk.Wpf.Controls

open System
open System.IO
open System.Reactive.Disposables
open System.Reactive.Linq
open System.Runtime.Serialization
open DotNetKit.FSharp
open Reactive.Bindings
open SharpFileSystem
open SharpFileSystem.FileSystems
open Tuktuk.Reactive.Bindings
open Tuktuk.Runtime.Serialization

[<Sealed>]
type Page
  ( fileSystem: IFileSystem
  , directoryPath: FileSystemPath
  , name: string
  ) =
  let disposables =
    new CompositeDisposable()

  let directoryPath =
    directoryPath |> ReactiveProperty.create
    |> tap disposables.Add

  let name =
    name |> ReactiveProperty.create
    |> tap disposables.Add

  let ancestorList =
    new AncestorList("path/to/directory")

  let fileTree =
    new FileTree()
    |> tap disposables.Add

  let fileList =
    new FileList(fileSystem, directoryPath.Value)
    |> tap disposables.Add

  new(fileSystem, directoryPath: FileSystemPath) =
    let name = directoryPath.EntityName
    new Page(fileSystem, directoryPath, name)

  new(fileSystem, directoryPath: string) =
    new Page(fileSystem, FileSystemPath.Parse(directoryPath))

  member this.Name =
    name

  member this.AncestorList =
    ancestorList

  member this.FileTree =
    fileTree

  member this.FileList =
    fileList

  member this.Dispose() =
    disposables.Dispose()

  interface ITabPage with
    override this.TabHeader =
      this.Name :> IReadOnlyReactiveProperty

  static member FromDirectory(directory: DirectoryInfo) =
    let fileSystem = new PhysicalFileSystem(directory.FullName)
    let directoryPath = FileSystemPath.Root
    Page(fileSystem, directoryPath)
