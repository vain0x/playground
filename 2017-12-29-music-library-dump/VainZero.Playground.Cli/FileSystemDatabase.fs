namespace VainZero.MusicLibrarian

open System
open System.IO
open VainZero.MusicLibrarian

module FileSystemDatabase =
  let private splitSlashed (str: string) =
    str.Split([|'/'|], StringSplitOptions.RemoveEmptyEntries)

  let private noneIfEmpty str =
    if String.IsNullOrWhiteSpace(str) then None else Some str

  let private tryClamp value lowerBound upperBound =
    if lowerBound <= value && value < upperBound then Some value else None

  let private tryParseInt32 (str: string) =
    match Int32.TryParse(str) with
    | (true, value) -> Some value
    | (false, _) -> None

  [<AbstractClass>]
  type RecursiveFileEnumerator<'T>() =
    abstract RootDirectory: DirectoryInfo
    abstract Choose: FileInfo -> option<'T>

    member private __.Recurses(dir: DirectoryInfo) =
      let name = dir.Name
      name <> ".git" && name <> "_bak" && name <> "_nobak"

    member this.EnumerateAsync(): Async<seq<'T>> =
      let rec go dir =
        async {
          let subdirs = Directory.GetDirectories(dir)
          let subfiles = Directory.GetFiles(dir)
          let! computations =
            seq {
              for subdir in subdirs do
                let path = Path.Combine(dir, subdir)
                if this.Recurses(DirectoryInfo(path)) then
                  yield go path
            }
            |> fun computations -> Async.Parallel(computations)
          return
            seq {
              yield! computations |> Seq.collect id

              for subfile in subfiles do
                let path = Path.Combine(dir, subfile)
                match this.Choose(FileInfo(path)) with
                | Some value ->
                  yield value
                | None -> ()
            }
        }
      go this.RootDirectory.FullName

  let private musicMetadata (mediaDirectory: DirectoryInfo) filePath (musicFile: TagLib.File): MusicMetadata =
    let tag = musicFile.Tag
    {
      Title =
        tag.Title
      Performers =
        tag.Performers |> Array.collect splitSlashed
      Composers =
        tag.Composers |> Array.collect splitSlashed
      Album =
        tag.Album |> noneIfEmpty
      TrackNumber =
        tag.Track |> int |> tryClamp 1 1000
      ReleaseYear =
        tag.Year |> int |> tryClamp 1000 2100
      FilePath =
        Path.GetRelativePath(mediaDirectory.FullName, filePath)
    }

  let musicRepository (mediaDirectory: DirectoryInfo) =
    let includedExtensions =
      [|
        ".mp3"
        ".m4a"
        ".flac"
      |]
      |> set

    let tryFindByPath (filePath: RelativeFilePath) =
      let extension = Path.GetExtension(filePath).ToLower()
      if includedExtensions |> Set.contains extension |> not then
        None
      else
        try
          use musicFile = TagLib.File.Create(filePath)
          let musicMetadata = musicMetadata mediaDirectory filePath musicFile
          Some musicMetadata
        with
        | ex ->
          eprintfn "Couldn't open media file %s" filePath
          eprintfn "%s" (ex |> string)
          None

    let findAll (): Async<MusicMetadata[]> =
      async {
        let musicDirectory =
          DirectoryInfo(Path.Combine(mediaDirectory.FullName, "Music"))
        let enumerator =
          { new RecursiveFileEnumerator<MusicMetadata>() with
              override __.RootDirectory =
                musicDirectory

              override __.Choose(file) =
                let extension = Path.GetExtension(file.Name).ToLower()
                if includedExtensions |> Set.contains extension then
                  tryFindByPath file.FullName
                else
                  None
          }
        let! musicMetadatas = enumerator.EnumerateAsync()
        return musicMetadatas |> Seq.toArray
      }

    { new MusicRepository() with
        override __.FindAll() =
          findAll ()
        override __.TryFindByPath(filePath: RelativeFilePath) =
          tryFindByPath filePath
    }

  let playlistRepository (mediaDirectory: DirectoryInfo) =
    let rootDirectory =
      DirectoryInfo(Path.Combine(mediaDirectory.FullName, "playlists"))

    let tryFindByPath (filePath: string) =
      try
        let source = File.ReadAllText(filePath)
        let location = Path.GetRelativePath(mediaDirectory.FullName, filePath)
        source |> Xspf.parse location |> Some
      with
      | ex ->
        eprintfn "Couldn't open playlist file %s" filePath
        eprintfn "%O" ex
        None

    let findAll () =
      async {
        let enumerator =
          { new RecursiveFileEnumerator<Playlist>() with
              override __.RootDirectory =
                rootDirectory
              override __.Choose(file) =
                tryFindByPath file.FullName
          }
        let! playlists = enumerator.EnumerateAsync()
        return playlists |> Seq.toArray
      }

    { new PlaylistRepository() with
        override __.FindAll() =
          findAll ()
        override __.TryFindByPath(filePath: RelativeFilePath) =
          tryFindByPath filePath
    }

  let create (mediaDirectory: DirectoryInfo) =
    {
      Database.MusicRepository =
        musicRepository mediaDirectory
      PlaylistRepository =
        playlistRepository mediaDirectory
    }
