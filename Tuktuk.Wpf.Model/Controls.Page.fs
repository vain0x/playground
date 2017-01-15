namespace Tuktuk.Wpf.Controls

open System
open System.IO
open System.Reactive.Disposables
open System.Reactive.Linq
open DotNetKit.FSharp
open Reactive.Bindings
open SharpFileSystem
open SharpFileSystem.FileSystems
open Tuktuk.Reactive.Bindings

type Page
  ( fileSystem: IFileSystem
  , directoryPath: FileSystemPath
  ) =
  let disposables =
    new CompositeDisposable()

  let directoryPath =
    directoryPath |> ReactiveProperty.create
    |> tap disposables.Add

  let name =
    directoryPath |> ReactiveProperty.map (fun path -> path.EntityName)
    :> IReadOnlyReactiveProperty<_>

  let ancestorList =
    new AncestorList("path/to/directory")

  let fileTree =
    new FileTree()

  let fileList =
    new FileList(fileSystem, directoryPath.Value)

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
