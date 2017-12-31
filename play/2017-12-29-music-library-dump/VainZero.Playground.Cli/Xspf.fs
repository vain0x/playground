namespace VainZero.MusicLibrarian

open System
open System.Linq
open System.Xml.Linq
open VainZero.FSharpErrorHandling

module Xspf =
  let inline xname str = XName.Get(str)

  let tryElementByTagName tagName (xe: XElement) =
    xe.Element(tagName) |> Option.ofObj

  let parse (location: RelativeFilePath) (source: string) =
    let parseTrack (element: XElement) =
      Option.build' {
        let text name =
          match element.Element(xname name) with
          | null -> None
          | e -> e.Value |> Some
        let! location =
          text "location"
        let title =
          text "title"
        let creators =
          text "creators"
          |> Option.defaultValue ""
          |> fun s -> s.Split([|'/'|], StringSplitOptions.RemoveEmptyEntries)

        return ()
      }

    let xdoc = XDocument.Parse(source)
    let tracks =
      xdoc.Descendants()
      |> Seq.choose (fun xe -> if xe.Name = xname "track" then Some xe else None)
      |> Seq.toArray
    {
      Location = location
      Tracks = tracks
    }

