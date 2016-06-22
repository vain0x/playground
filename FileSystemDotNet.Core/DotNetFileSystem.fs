namespace FileSystemDotNet.Core

open System.IO
open System.Text

type DotNetFileInfo(_file: FileInfo) =
  let _deletedEvent = Event<_, _>()

  new (path: string) =
    DotNetFileInfo(FileInfo(path))

  interface IFile with
    member this.Name = _file.Name

    member this.Parent =
      (DotNetDirectoryInfo(_file.Directory) :> IDirectory) |> Some

    member this.Attributes = _file.Attributes

    member this.Exists = _file.Exists

    member this.Create() =
      use stream = _file.Create() in ()

    member this.Delete() =
      _file.Delete()
      _deletedEvent.Trigger(this, null)

    [<CLIEvent>]
    member this.Deleted = _deletedEvent.Publish

    member this.ReadTextAsync() =
      async {
        let stream = _file.OpenText()
        let! text = stream.ReadToEndAsync() |> Async.AwaitTask
        do stream.Dispose()
        return text
      }

    member this.WriteTextAsync(text) =
      async {
        let stream = _file.OpenWrite()
        do! stream.AsyncWrite(UTF8Encoding.UTF8.GetBytes(text))
        do stream.Dispose()
      }

and DotNetDirectoryInfo(_dir: DirectoryInfo) =
  let _deletedEvent = Event<_, _>()

  new (path: string) =
    DotNetDirectoryInfo(DirectoryInfo(path))

  interface IDirectory with
    member this.Name = _dir.Name

    member this.Parent =
      _dir.Parent |> Option.ofObj
      |> Option.map (fun dir -> DotNetDirectoryInfo(dir) :> IDirectory)

    member this.Attributes = _dir.Attributes

    member this.Exists = _dir.Exists

    member this.Delete() =
      _dir.Delete()
      _deletedEvent.Trigger(this, null)

    member this.Create() =
      _dir.Create()

    [<CLIEvent>]
    member this.Deleted = _deletedEvent.Publish

    member this.GetFiles() =
      _dir.GetFiles()
      |> Array.map (fun file -> DotNetFileInfo(file) :> IFile)

    member this.GetDirectories() =
      _dir.GetDirectories()
      |> Array.map (fun dir -> DotNetDirectoryInfo(dir) :> IDirectory)

    member this.AddFiles(files) = ()

    member this.AddDirectories(dirs) = ()

type DotNetFileSystem private() =
  static member val private LazyInstance = lazy DotNetFileSystem()
  static member Instance = DotNetFileSystem.LazyInstance.Value

  interface IFileSystem with
    member this.FileInfo(path) =
      DotNetFileInfo(path) :> IFile

    member this.DirectoryInfo(path) =
      DotNetDirectoryInfo(path) :> IDirectory
