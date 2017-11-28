namespace FileSystemDotNet.Core

open System
open System.IO

type IFileBase =
  abstract member Name: string
  abstract member Parent: option<IDirectory>
  abstract member Attributes: FileAttributes
  abstract member Exists: bool
  abstract member Create: unit -> unit
  abstract member Delete: unit -> unit

  [<CLIEvent>]
  abstract member Deleted: IEvent<EventHandler, EventArgs>

and IFile =
  inherit IFileBase

  abstract member ReadTextAsync: unit -> Async<string>
  abstract member WriteTextAsync: string -> Async<unit>

and IDirectory =
  inherit IFileBase
  
  abstract member GetFiles: unit -> IFile []
  abstract member GetDirectories: unit -> IDirectory []

  abstract member AddFiles: array<IFile> -> unit
  abstract member AddDirectories: array<IDirectory> -> unit

type IFileSystem =
  abstract member FileInfo: string -> IFile
  abstract member DirectoryInfo: string -> IDirectory
