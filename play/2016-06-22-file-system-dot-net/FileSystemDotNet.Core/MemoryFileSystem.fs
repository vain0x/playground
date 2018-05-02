namespace FileSystemDotNet.Core

open System
open System.IO
open FParsec

type MemoryFileBase(_name: string, _parent: option<IDirectory>, _attributes: FileAttributes) =
  let _deletedEvent = Event<_, _>()

  let mutable _exists = true

  abstract member Create: unit -> unit
  default this.Create() =
    _exists <- true

  abstract member Delete: unit -> unit
  default this.Delete() =
    _exists <- false
    _deletedEvent.Trigger(this, null)

  abstract member Attributes: FileAttributes
  default this.Attributes = _attributes

  interface IFileBase with
    member this.Name        = _name
    member this.Parent      = _parent
    member this.Attributes  = this.Attributes
    member this.Exists      = _exists
    member this.Create()    = this.Create()
    member this.Delete()    = this.Delete()

    [<CLIEvent>]
    member this.Deleted     = _deletedEvent.Publish

type MemoryFile(_name: string, _parent: IDirectory, _attributes: FileAttributes) =
  inherit MemoryFileBase(_name, Some _parent, _attributes)

  let mutable _content = ""

  new (name, parent) =
    MemoryFile(name, parent, FileAttributes.Normal)

  interface IFile with
    override this.Create() =
      _content <- ""
      if not (this :> IFile).Exists then
        this.Create()
        _parent.AddFiles([|this :> IFile|])

    override this.Delete() =
      if (this :> IFile).Exists then
        this.Delete()

    member this.ReadTextAsync() =
      if (this :> IFile).Exists
      then async { return _content }
      else raise (FileNotFoundException())

    member this.WriteTextAsync(text) =
      if (this :> IFile).Exists then
        do _content <- text
        async { return () }
      else raise (FileNotFoundException())

and MemoryDirectory
  ( _name: string
  , _parent: option<IDirectory>
  , _attributes: FileAttributes
  ) =
  inherit MemoryFileBase(_name, _parent, _attributes)

  let mutable _subfiles = [||]
  let mutable _subdirs = [||]

  new (name, parent) =
    MemoryDirectory(name, parent, FileAttributes.Directory)

  interface IDirectory with
    member this.GetFiles() = _subfiles
    member this.GetDirectories() = _subdirs

    override this.Attributes =
      this.Attributes ||| FileAttributes.Directory

    override this.Create() =
      if not (this :> IDirectory).Exists then
        this.Create()
        _parent |> Option.iter (fun parent ->
          parent.AddDirectories([|this :> IDirectory|])
          )

    override this.Delete() =
      if (this :> IDirectory).Exists then
        this.Delete()

    member this.AddFiles(files) =
      _subfiles <- Array.append _subfiles files
      files |> Array.iter (fun file ->
        file.Deleted.Add(fun _ ->
          _subfiles <- _subfiles |> Array.filter (fun f -> f.Name <> file.Name)
        ))

    member this.AddDirectories(dirs) =
      _subdirs <- Array.append _subdirs dirs
      dirs |> Array.iter (fun dir ->
        dir.Deleted.Add(fun _ ->
          _subdirs <- _subdirs |> Array.filter (fun d -> d.Name <> dir.Name)
        ))

type MemoryFileSystem(_roots: array<IDirectory>) =
  interface IFileSystem with
    /// Example: @"R\path\to\dir"
    member this.DirectoryInfo(path) =
      let rec loop dir =
        function
        | [] | [""] ->
          dir
        | name :: path ->
          let dir =
            match dir |> Directory.tryFindDirectory name with
            | Some dir -> dir
            | None ->
              let dir = MemoryDirectory(name, Some dir)
              dir.Delete()
              dir :> IDirectory
          in loop dir path
      match path.Split(Path.DirectorySeparatorChar) |> Array.toList with
      | [] -> raise (ArgumentException())
      | rootName :: path ->
          let root = _roots |> Array.find (fun dir -> dir.Name = rootName)
          in loop root path

    member this.FileInfo(path) =
      let dir = (this :> IFileSystem).DirectoryInfo(Path.GetDirectoryName(path))
      let name = Path.GetFileName(path)
      match dir |> Directory.tryFindFile name with
      | Some file -> file
      | None ->
        let file = MemoryFile(name, dir)
        file.Delete()
        file :> IFile

  member this.Roots = _roots

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MemoryFileSystem =
  module Parser =
    open FParsec
    open FParsec.CharParsers
    open FParsec.Primitives

    module Detail =
      type FileOrDirectory =
        | File          of IFile
        | Dir           of IDirectory

      let (fileList: Parser<array<IFile> * array<IDirectory>, _>, fileListRef) =
        createParserForwardedToRef ()

      let fileName =
        many1Chars (noneOf " \t\n/\\{}")

      let nondirectoryFile =
        parse {
          let! name = fileName
          let! parentOpt = getUserState
          return (MemoryFile(name, parentOpt |> Option.get) :> IFile) |> File
        }

      let directory =
        parse {
          let! name = fileName
          let! parentOpt = getUserState
          do! spaces >>. skipChar '{' .>> spaces
          let dir = MemoryDirectory(name, parentOpt) :> IDirectory
          do! setUserState (Some dir)
          let! (subfiles, subdirs) = fileList
          do! spaces >>. skipChar '}'
          do! setUserState parentOpt
          do dir.AddFiles(subfiles)
          do dir.AddDirectories(subdirs)
          return Dir dir
        }

      let file =
        (attempt directory)
        <|> nondirectoryFile

      fileListRef :=
        sepEndBy file spaces
        |>> (fun files ->
          let subfiles = files |> List.choose (function (File file) -> Some file | _ -> None)
          let subdirs  = files |> List.choose (function (Dir dir) -> Some dir | _ -> None)
          in (subfiles |> List.toArray, subdirs |> List.toArray)
          )

      let fileSystem =
        spaces >>. fileList .>> spaces .>> eof
        |>> (fun (_, roots) -> MemoryFileSystem(roots))

    open Detail

    let parse name source =
      match runParserOnString fileSystem None name source with
      | Success (fsys, _, _)    -> fsys
      | Failure (msg, _, _)     -> failwith msg
