module HashWeb.Markdown

open System.IO
open CommonMark

let private normalizeLf (s: string) = s.Replace("\r\n", "\n")

let private toLines (s: string) =
  (s |> normalizeLf).TrimEnd().Split("\n")
  |> Array.toList

// -----------------------------------------------
// Front matter
// -----------------------------------------------

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

// -----------------------------------------------
// Hash tag
// -----------------------------------------------

let private tokenize (text: string) =
  let isSpace = System.Char.IsWhiteSpace
  let len = text.Length

  let rec skipSpaces (i: int) =
    if i < len && isSpace text.[i] then
      i + 1
    else
      i

  let rec skipWord (i: int) =
    if i = len || isSpace text.[i] then
      i
    else
      skipWord (i + 1)

  let rec go acc i =
    if i < len then
      let r =
        if isSpace text.[i] then
          skipSpaces i
        else
          skipWord i

      assert (i < r && r <= len)
      go ((i, r) :: acc) r
    else
      List.rev acc

  go [] 0

let private preprocessHashTags (text: string) =
  let lines, _ =
    text
    |> toLines
    |> List.mapFold
         (fun inFence line ->
           if line |> String.startsWith "```" then
             (true, line), not inFence
           else
             (inFence, line), inFence)
         false

  let lines, acc =
    lines
    |> List.mapFold
         (fun acc (inFence, line) ->
           if inFence then
             line, acc
           else
             let words, acc =
               line
               |> tokenize
               |> List.mapFold
                    (fun acc (l, r) ->
                      assert (0 <= l && l < r && r <= line.Length)

                      if r - l >= 2
                         && line.[l] = '#'
                         && line.[l + 1] <> '#' then
                        let word =
                          let i = l + 1
                          line.Substring(i, r - i)

                        "{{#" + word + "#}}", word :: acc

                      else
                        line.Substring(l, r - l), acc)
                    acc

             let line = words |> String.concat ""
             line, acc)
         []

  String.concat "\n" lines, acc

// -----------------------------------------------
// Contents
// -----------------------------------------------

let private settings =
  let s = CommonMarkSettings.Default
  s.AdditionalFeatures <- CommonMarkAdditionalFeatures.StrikethroughTilde
  s.RenderSoftLineBreaksAsLineBreaks <- false
  s

let parse (markdown: string) =
  let front, text = parseFrontMatter markdown
  let text, hashTags = text |> preprocessHashTags

  let contents =
    CommonMark.CommonMarkConverter.Parse(text, settings)

  front, hashTags, contents

let toHtml (block: Syntax.Block) : string =
  use buf = new StringWriter()

  Formatters
    .HtmlFormatter(buf, settings)
    .WriteDocument(block)

  buf.ToString()
