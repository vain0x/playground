namespace VainZero.MusicLibrarian

open System
open System.Linq
open System.Xml.Linq

type Xspf() =
  member this.Parse(source: string) =
    let xname = XName.Get

    let trackFromTrackElement (element: XElement) =
      let title = element.Element(xname "title")

      match element.Element(xname "location") with
      | null ->
        None
      | element ->
        element.Value |> Some

    let xdoc = XDocument.Parse(source)
    xdoc.Descendants()
    |> Seq.filter (fun element -> element.Name = "track")
    |> Seq.map trackFromTrackElement


