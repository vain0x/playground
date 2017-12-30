namespace VainZero.MusicLibrarian

open System
open System.IO
open VainZero.MusicLibrarian

module FileSystemDatabase =
  let private noneIfEmpty str =
    if String.IsNullOrWhiteSpace(str) then None else Some str

  let private tryParseInt32 (str: string) =
    match Int32.TryParse(str) with
    | (true, value) -> Some value
    | (false, _) -> None

  [<AbstractClass>]
  type RecursiveFileEnumerator<'T>() =
    abstract RootDirectory: DirectoryInfo
    abstract Choose: FileInfo -> option<'T>

    member private this.Recurses(dir: DirectoryInfo) =
      let name = dir.Name
      name <> ".git" && name <> "_bak" && name <> "_nobak"

    member this.Enumerate() =
      let rec go dir =
        [|
          for subdir in Directory.GetDirectories(dir) do
            let path = Path.Combine(dir, subdir)
            if this.Recurses(DirectoryInfo(path)) then
              yield! go path

          for fileName in Directory.GetFiles(dir) do
            let path = Path.Combine(dir, fileName)
            match this.Choose(FileInfo(path)) with
            | Some value ->
              yield value
            | None -> ()
        |]
      go this.RootDirectory.FullName

  let private musicMetadata mediaDirectoryPath (musicFile: TagLib.File) =
    let tag = musicFile.Tag
    {
      Title =
        tag.Title
      Performers =
        tag.Performers
      Composers =
        tag.Composers
      Album =
        tag.Album |> noneIfEmpty
      TrackNumber =
        tag.Track |> int |> Some
      ReleaseYear =
        let year = tag.Year |> int
        if 1000 <= year && year < 2100 then Some year else None
      FilePath =
        Path.GetRelativePath(musicFile.Name, mediaDirectoryPath)
    }

  let musicRepository (mediaDirectory: string) =
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
          let musicMetadata = musicMetadata mediaDirectory musicFile
          Some musicMetadata
        with
        | ex ->
          eprintfn "Couldn't open media file %s" filePath
          eprintfn "%s" (ex |> string)
          None

    let findAll () =
      let musicDirectory =
        DirectoryInfo(Path.Combine(mediaDirectory, "Music"))
      let enumerator =
        { new RecursiveFileEnumerator<MusicMetadata>() with
            override this.RootDirectory =
              musicDirectory

            override this.Choose(file) =
              let extension = Path.GetExtension(file.Name).ToLower()
              if includedExtensions |> Set.contains extension then
                tryFindByPath file.FullName
              else
                None
        }
      enumerator.Enumerate()

    { new MusicRepository() with
        override __.FindAll() =
          findAll ()

        override __.TryFindByPath(filePath: RelativeFilePath) =
          tryFindByPath filePath
    }

  let playlistRepository (mediaDirectory: string) =
    let findAll () =
      let enumerator =
        { new RecursiveFileEnumerator<Playlist> with
            override this.RootDirectory =
              rootDirectory

            override this.Choose(file) =
              try
                Xspf.Parse()
              with
              | ex ->
                eprintfn "Couldn't open playlist file %s" file.FullName
                eprintfn "%O" ex
        }

  let create (mediaDirectory: string) =
    {
      MusicRepository =
        musicRepository mediaDirectory
      PlaylistRepository =
        playlistRepository mediaDirectory
    }
