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
  ( directoryPath: ReactiveProperty<FileSystemPath>
  , name: ReactiveProperty<string>
  , ancestorList: AncestorList
  , fileTree: FileTree
  , fileList: FileList
  , launchCommand: ReactiveCommand<FileSystemPath>
  , disposables: CompositeDisposable
  ) =
  new(fileSystem, directoryPath: FileSystemPath, name) =
    let disposables =
      new CompositeDisposable()
    let directoryPath =
      directoryPath |> ReactiveProperty.create
      |> tap disposables.Add
    let launchCommand =
      new ReactiveCommand<FileSystemPath>()
      |> tap disposables.Add
    do
      launchCommand.Where(fun path -> path.IsDirectory) |> Observable.subscribe
        (fun path -> directoryPath.Value <- path)
      |> disposables.Add
    new Page
      ( directoryPath
      , name |> ReactiveProperty.create
        |> tap disposables.Add
      , new AncestorList("path/to/directory")
      , new FileTree()
        |> tap disposables.Add
      , new FileList(fileSystem, directoryPath.Value, launchCommand)
        |> tap disposables.Add
      , launchCommand
      , disposables
      )

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

  member this.LaunchCommand =
    launchCommand

  member this.Dispose() =
    disposables.Dispose()

  interface ITabPage with
    override this.TabHeader =
      this.Name :> IReadOnlyReactiveProperty

  static member FromDirectory(directory: DirectoryInfo) =
    let fileSystem = new PhysicalFileSystem(directory.FullName)
    let directoryPath = FileSystemPath.Root
    Page(fileSystem, directoryPath)
