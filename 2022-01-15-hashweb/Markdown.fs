module HashWeb.Markdown

open System.IO
open CommonMark

let private normalizeLf (s: string) = s.Replace("\r\n", "\n")

let private toLines (s: string) =
  (s |> normalizeLf).TrimEnd().Split("\n")
  |> Array.toList

let private extractFrontMatter (text: string) =
  let isSep s = s |> String.startsWith "---"

  let lines =
    text
    |> toLines
    |> List.skipWhile (fun s -> s |> isSep |> not)

  match lines with
  | line :: lines when isSep line ->
    let matter =
      lines
      |> List.takeWhile (fun s -> s |> isSep |> not)

    let lines =
      let lines =
        lines |> List.skip (matter |> List.length)

      match lines with
      | line :: lines when isSep line -> lines
      | _ -> lines

    Some matter, String.concat "\n" lines

  | _ -> None, text

let private parseYamlSimple (lines: string list) =
  lines
  |> List.filter (fun s ->
    s |> String.startsWith "#" |> not
    && s.Trim() <> "")
  |> List.choose (fun s ->
    let i = s.IndexOf(":")

    if i < 0 then
      None
    else
      let key = s.Substring(0, i).Trim()
      let value = s.Substring(i + 1, s.Length).Trim()

      let value =
        if value |> String.startsWith "'"
           && value |> String.endsWith "'" then
          value.Substring(1, value.Length - 2)
        else if value |> String.startsWith "\""
                && value |> String.endsWith "\"" then
          value.Substring(1, value.Length - 2)
        else
          value

      Some(key, value))

let private parseFrontMatter (text: string) =
  let frontOpt, text = extractFrontMatter text

  let front =
    match frontOpt with
    | Some front -> parseYamlSimple front
    | _ -> []

  front, text

let private settings =
  let s = CommonMarkSettings.Default
  s.AdditionalFeatures <- CommonMarkAdditionalFeatures.StrikethroughTilde
  s.RenderSoftLineBreaksAsLineBreaks <- false
  s

let parse (text: string) =
  let front, text = parseFrontMatter text

  let contents =
    CommonMark.CommonMarkConverter.Parse(text, settings)

  front, contents

let toHtml (block: Syntax.Block) : string =
  use buf = new StringWriter()

  Formatters
    .HtmlFormatter(buf, settings)
    .WriteDocument(block)

  buf.ToString()
