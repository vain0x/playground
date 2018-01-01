namespace VainZero.MusicLibrarian

open System
open System.Linq
open System.Xml.Linq
open VainZero.FSharpErrorHandling

module Xspf =
  let inline xname str = XName.Get(str)

  let parseTrack (element: XElement): option<MusicTrack> =
    Option.build {
      let text name =
        element.Elements()
        |> Seq.tryFind (fun xe -> xe.Name.LocalName = name)
        |> Option.map (fun xe -> xe.Value)
      let! location =
        text "location"
      let title =
        text "title"
        |> Option.defaultValue ""
      let creator =
        text "creator"
        |> Option.defaultValue ""
      let album =
        text "album"
        |> Option.defaultValue ""
      return
        {
          Location = location
          Title = title
          Creator = creator
          Album = album
        }
    }

  let parse (location: RelativeFilePath) (source: string): Playlist =
    let xdoc = XDocument.Parse(source)
    let tracks =
      xdoc.Descendants()
      |> Seq.filter (fun xe -> xe.Name.LocalName = "track")
      |> Seq.choose parseTrack
      |> Seq.toArray
    {
      Location = location
      Tracks = tracks
    }
