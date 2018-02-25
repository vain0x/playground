namespace VainZero.LyricsParser

open System
open System.Text
open VainZero.LyricsParser.Ast

module Metadata =
  let trim (str: string) = str.Trim()
  let trimEnd (str: string) = str.TrimEnd()
  let trimArray = Array.map trim
  let parseInt value =
    match Int32.TryParse(value |> trimEnd) with
    | (true, value) ->
      value
    | (false, _) ->
      failwithf "Can't parse as integer: '%s'." value

  let ofTitle title: Metadata =
    {
      Title = title
      Writers = Array.empty
      Composers = Array.empty
      Performers = Array.empty
      TrackNumber = None
      ReleaseYear = None
      Note = ""
      Tie = Array.empty
      Entries = Map.empty
    }

  let update (key, valueList, rawValue) (metadata: Metadata) =
    match key with
    | "作曲" ->
      { metadata with Composers = trimArray valueList }
    | "作詞" ->
      { metadata with Writers = trimArray valueList }
    | "歌" | "演奏" ->
      { metadata with Performers = trimArray valueList }
    | "TrackNumber" ->
      { metadata with TrackNumber = rawValue |> parseInt |> Some }
    | "Release" ->
      { metadata with ReleaseYear = rawValue |> parseInt |> Some }
    | "Note" ->
      { metadata with Note = metadata.Note + trimEnd rawValue + Environment.NewLine }
    | "Tie" ->
      { metadata with Tie = Array.append metadata.Tie [|trimEnd rawValue|] }
    | _ ->
      { metadata with
          Entries =
            Map.add
              key
              (Array.append
                (metadata.Entries
                  |> Map.tryFind key
                  |> Option.defaultValue Array.empty
                ) [|trimEnd rawValue|]
              )
              metadata.Entries
      }

  let updateMany (keys, valueList, rawValue) (metadata: Metadata) =
    keys |> Array.fold (fun metadata key -> metadata |> update (key, valueList, rawValue)) metadata

module Parsing =
  type SongState =
    | MHeader
      of Metadata
    | MBody
      of Metadata * StringBuilder

  module SongState =
    let ofTitle title =
      MHeader (Metadata.ofTitle title)

    let build =
      function
      | MHeader metadata ->
        { Metadata = metadata; Lyrics = "" }
      | MBody (metadata, lyrics) ->
        { Metadata = metadata; Lyrics = lyrics.ToString() }

  type ParentState =
    | PSGround
    | PSAlbum
      of Metadata * Song[]

  type State =
    | SGround
    | SAlbum
      of Metadata
    | SSong
      of ParentState * SongState

  let (|SingleSongTitle|AlbumTitle|AlbumSongTitle|NormalLine|EmptyLine|) (line: string) =
    if String.IsNullOrWhiteSpace(line) then
      EmptyLine ""
    else if line.StartsWith("＊『") && line.EndsWith("』") then
      AlbumTitle (line.Substring(2, line.Length - 3))
    else if line.StartsWith("＊") then
      SingleSongTitle (line.Substring(1, line.Length - 1))
    else if line.StartsWith("－") then
      AlbumSongTitle (line.Substring(1, line.Length - 1))
    else
      NormalLine line

  let trimStartSoft (str: string) =
    let spaceCount = str.ToCharArray() |> Seq.takeWhile (fun c -> c = ' ') |> Seq.length
    str.Substring(spaceCount)

  let parseHeaderLine (line: string) =
    let headerLength = line.IndexOf("：")
    if line.StartsWith("#(") && line.EndsWith(")") && line.IndexOf(":") >= 0 then
      let i = line.IndexOf(":")
      let key = line.Substring(2, i - 2).Trim()
      let value = line.Substring(i + 1, line.Length - i - 2) |> trimStartSoft
      Some (Metadata.updateMany ([|key|], [|value|], value))
    // #NN is track-number.
    else if line.StartsWith("#") && (Int32.TryParse(line.Substring(1)) |> fst) then
      let value = line.Substring(1)
      Some (Metadata.update ("TrackNumber", [|value|], value))
    else if line.StartsWith("#") then
      let value = line.Substring(1) |> trimStartSoft
      Some (Metadata.update ("Note", [|value|], value))
    else if headerLength >= 0 then
      let heading = line.Substring(0, headerLength)
      let body = line.Substring(headerLength + 1)
      let keys = heading.Split("・")
      let values = body.Split(";")
      Some (Metadata.updateMany (keys, values, body))
    else
      None

  let parse (source: string): Result<TrackList, string> =
    let lines = source.Split([|'\r'; '\n'|])
    let tracks = ResizeArray()
    let mutable state = SGround
    try
      let pushCurrent lineIndex =
        match state with
        | SGround -> ()
        | SAlbum metadata ->
          eprintfn "Unexpected end of album header at %d." (1 + lineIndex)
          let album = AlbumTrack (metadata, Array.empty)
          tracks.Add(album)
        | SSong (parent, songState) ->
          let song = songState |> SongState.build
          match parent with
          | PSGround ->
            tracks.Add(song |> SongTrack)
          | PSAlbum (metadata, songs) ->
            let songs = [|yield! songs; yield song|]
            let album = AlbumTrack (metadata, songs)
            tracks.Add(album)
      let rec go i =
        if i < lines.Length then
          try
            let line = lines.[i]
            match (state, line) with
            | (_, AlbumTitle albumTitle) ->
              pushCurrent i
              state <- SAlbum (Metadata.ofTitle albumTitle)

            | (_, SingleSongTitle songTitle) ->
              pushCurrent i
              let songState = SongState.ofTitle songTitle
              state <- SSong (PSGround, songState)

            | (SAlbum metadata, AlbumSongTitle songTitle) ->
              let songState = SongState.ofTitle songTitle
              state <- SSong (PSAlbum (metadata, Array.empty), songState)

            | (SSong (PSAlbum (metadata, songs), last), AlbumSongTitle songTitle) ->
              let songs = Array.append songs [|last |> SongState.build|]
              let songState = SongState.ofTitle songTitle
              state <- SSong (PSAlbum (metadata, songs), songState)

            | (SAlbum metadata, NormalLine line) ->
              match line |> parseHeaderLine with
              | Some updateMetadata ->
                let metadata = metadata |> updateMetadata
                state <- SAlbum metadata
              | None ->
                eprintfn "#%d Can't parse header line '%s'." (i + 1) line

            | (SSong (parent, MHeader metadata), NormalLine line) ->
              match line |> parseHeaderLine with
              | Some updateMetadata ->
                let metadata = metadata |> updateMetadata
                state <- SSong (parent, MHeader metadata)
              | None ->
                eprintfn "#%d Can't parse header line '%s'." (i + 1) line

            | (SSong (parent, MHeader metadata), EmptyLine _) ->
              state <- SSong (parent, MBody (metadata, StringBuilder()))

            | (SSong (_, MBody (_, lyrics)), (EmptyLine line | NormalLine line)) ->
              lyrics.AppendLine(line) |> ignore

            // Trivial cases.
            | ((SGround | SAlbum _), EmptyLine _) ->
              ()

            // Error cases.
            | (SGround, NormalLine _) ->
              eprintfn "#%d Ignoring line out of context: '%s'." (i + 1) line
            | ((SGround | SSong (PSGround, _)), AlbumSongTitle _) ->
              eprintfn "#%d Album song title appeared out of album: '%s'." (i + 1) line
          with
          | ex ->
            failwithf "#%d Unknown error: \n%s" (i + 1) (string ex)
          go (i + 1)
        else
          pushCurrent i
      go 0
      Ok { Tracks = tracks.ToArray() }
    with
    | e ->
      eprintfn "%s" (string e)
      Error e.Message
