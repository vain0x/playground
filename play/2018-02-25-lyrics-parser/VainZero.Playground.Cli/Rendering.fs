namespace VainZero.LyricsParser

open System
open System.Text
open System.Text.RegularExpressions
open VainZero.LyricsParser.Ast

module Rendering =
  let eol = Environment.NewLine

  module Toml =
    type TomlStr =
      private
      | TomlStr of string

    let unwrap (TomlStr value) = value

    let (|TomlStr|) (TomlStr value) = value

    let tomlifyKey =
      let regex = new Regex("""^[a-z-]+$""")
      fun (value: string) ->
        if regex.IsMatch(value) |> not then
          failwith "Unexpected key."
        else
          TomlStr value

    let tomlifyMultilineString (value: string) =
      //let last = if value.EndsWith("\n") then "" else "\\" + eol
      "\"\"\"\\" + eol + value.TrimEnd() + eol + "\"\"\"" |> TomlStr

    let tomlifyString (value: string) =
      if value.Contains("\n") || value.Contains("\r") then
        tomlifyMultilineString value
      else if value.Contains("\\\"") && value.Contains("'") then
        value.Replace("\"", "\\\"") |> fun str -> "\"" + str + "\"" |> TomlStr
      else if value.Contains("\\\"") then
        "'" + value + "'" |> TomlStr
      else
        "\"" + value + "\"" |> TomlStr

    let tomlifyInt (value: int) =
      string value |> TomlStr

    let tomlifyFlowArray (items: TomlStr[]) =
      sprintf "[%s]" (items |> Array.map unwrap |> String.concat ", ")
      |> TomlStr

  open Toml

  let renderToml (trackList: TrackList): string =
    let out = StringBuilder()

    let fieldsFromMetadata (metadata: Metadata) =
      let (-->!) x y =
        y |> Option.map (fun y -> (x, y))
      let (-->) x y = (tomlifyKey x) -->! y
      let ofString x =
        if x |> String.IsNullOrWhiteSpace then
          None
        else
          x |> tomlifyString |> Some
      let ofMultilineString x =
        if x |> String.IsNullOrWhiteSpace then
          None
        else
          x |> tomlifyMultilineString |> Some
      let ofStringArray xs =
        if xs |> Array.isEmpty then
          None
        else
          xs |> Array.map tomlifyString |> tomlifyFlowArray |> Some
      let ofIntOption x =
        x |> Option.map tomlifyInt
      [|
        yield "title" --> (metadata.Title |> ofString)
        yield "composers" --> (metadata.Composers |> ofStringArray)
        yield "writers" --> (metadata.Writers |> ofStringArray)
        yield "performers" --> (metadata.Performers |> ofStringArray)
        yield "track-number" --> (metadata.TrackNumber |> ofIntOption)
        yield "release-year" --> (metadata.ReleaseYear |> ofIntOption)
        yield "tie" --> (metadata.Tie |> ofStringArray)

        for KeyValue (key, value) in metadata.Entries do
          yield tomlifyString ("-" + key) -->! (value |> ofStringArray)

        yield "note" --> (metadata.Note |> ofMultilineString)
      |]

    let writeBlank () =
      out.AppendLine() |> ignore

    let write (TomlStr key) (TomlStr value) =
      out.AppendLine(sprintf "%s = %s" key value) |> ignore

    let writeString key (value: string) =
      write key (tomlifyString value)

    let writeFlowArray key values =
      if values |> Array.isEmpty |> not then
        write key (tomlifyFlowArray values)

    let writeMetadata (metadata: Metadata) =
      for (key, value) in metadata |> fieldsFromMetadata |> Seq.choose id do
        write key value

    let writeLyrics (lyrics: string) =
      if lyrics |> String.IsNullOrWhiteSpace |> not then
          writeBlank ()
          write (tomlifyKey "lyrics") (lyrics |> tomlifyMultilineString)

    for (i, track) in trackList.Tracks |> Seq.indexed do
      if i > 0 then writeBlank ()
      out.AppendLine("[[entries]]") |> ignore
      match track with
      | SongTrack song ->
        writeMetadata song.Metadata
        writeLyrics song.Lyrics

      | AlbumTrack (metadata, songs) ->
        out.AppendLine("type = \"album\"") |> ignore
        writeMetadata metadata
        for song in songs do
          writeBlank ()
          out.AppendLine("[[entries.tracks]]") |> ignore
          writeMetadata song.Metadata
          writeLyrics song.Lyrics

    out.ToString()

    (*

    let output =
      sprintf """
[trackList]
title = "%s"
"""
        trackList.Title
    output.TrimStart()
    *)
