module HashWeb.Program

open System
open System.IO

let private help =
  """hashweb v0.1.0

SUBCOMMANDS
    build     Build files.
"""

let private context hint action =
  try
    action ()
  with
  | ex -> raise (exn (hint, ex))

let private dirname (s: string) = Path.GetDirectoryName(s)
let private basename (s: string) = Path.GetFileName(s)
let private getStem (s: string) = Path.GetFileNameWithoutExtension(s)
let private getExt (s: string) = Path.GetExtension(s)

let private readDir (dir: string) =
  if not (Directory.Exists(dir)) then
    failwithf "File not found '%s'" dir
  else
    let files = Directory.GetFiles(dir) |> Array.toList

    let subdirs =
      Directory.GetDirectories(dir) |> Array.toList

    files, subdirs

let private readText (filename: string) =
  if not (File.Exists filename) then
    failwithf "File not found '%s'" filename
  else
    File.ReadAllText(filename)

let private createDir (dir: string) =
  Directory.CreateDirectory(dir) |> ignore

let private writeFile filename contents =
  let same =
    File.Exists(filename)
    && (try
          File.ReadAllText(filename) = contents
        with
        | _ -> false)

  if not same then
    File.WriteAllText(filename, contents)

/// Absolute path to a directory.
type private BaseDir = BaseDir of string
let private basedRoot (BaseDir dir) = dir
let private basedPath (BaseDir l) r = Path.Combine(l, r)

[<RequireQualifiedAccess>]
type private SiteConfig = { Base: string; Author: string }

module private SiteConfig =
  let private empty: SiteConfig = { Base = "/"; Author = "John Doe" }

[<RequireQualifiedAccess>]
type private PageConfig = { Title: string; Slug: string }

module private PageConfig =
  let private empty: PageConfig = { Title = ""; Slug = "" }

let private dirIsExcluded (dir: string) =
  let name = basename dir

  match name with
  | "node_modules"
  | "target"
  | "Debug"
  | "Release" -> true

  | _ ->
    name |> String.startsWith "."
    || name |> String.contains "~"

module private Sources =
  let findFiles (projectDir: BaseDir) =
    let rec collect acc depth dir =
      if dirIsExcluded dir then
        acc
      else
        let files, subdirs = readDir dir
        let acc = files :: acc

        if depth < 3 then
          subdirs
          |> List.fold (fun acc subdir -> collect acc (depth + 1) subdir) acc
        else
          acc

    basedPath projectDir "pages"
    |> collect [] 0
    |> List.collect id

[<RequireQualifiedAccess>]
type private Page =
  { Title: string
    Slug: string
    HashTags: string list
    Contents: string }

let private parsePageFile filename text : Page =
  { Title = getStem filename
    Slug = getStem filename
    HashTags = []
    Contents = text }

[<RequireQualifiedAccess>]
type private PageRenderParams = { Title: string; Contents: string }

let private renderHtml (p: PageRenderParams) =
  """<!DOCTYPE html>
<html lang="ja">

<head>
  <meta charset="UTF-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>${TITLE}</title>
</head>

<body>
  <header>
    <h1>${TITLE}</h1>
  </header>

  <main>
    ${CONTENTS}
  </main>
</body>

</html>"""
  |> String.replace "${TITLE}" p.Title
  |> String.replace "${CONTENTS}" p.Contents

let private cmdBuild () : unit =
  let projectDir = Environment.CurrentDirectory |> BaseDir
  let outputDir = basedPath projectDir "dist" |> BaseDir

  let inputFiles =
    Sources.findFiles projectDir
    |> List.filter (fun filename -> getExt filename = ".md")
    |> List.map (fun filename -> filename, readText filename)

  let inputPages =
    inputFiles
    |> List.map (fun (filename, contents) ->
      let page = parsePageFile filename contents

      let meta, hashTags, markdownContents = page.Contents |> Markdown.parse

      let page =
        { page with
            HashTags = hashTags
            Contents = Markdown.toHtml markdownContents }

      let page =
        meta
        |> List.fold
             (fun (page: Page) (key, value) ->
               match key with
               | "title" -> { page with Title = value }
               | "slug" -> { page with Slug = value }
               | _ ->
                 eprintfn "WARN: %s Unknown property '%s'." filename key
                 page)
             page

      page)

  let titleMap =
    inputPages
    |> List.map (fun (page: Page) -> page.Slug, page.Title)
    |> Map.ofList

  let inputPages =
    inputPages
    |> List.map (fun (page: Page) ->
      let contents =
        page.HashTags
        |> List.fold
             (fun contents hashTag ->
               let link =
                 match titleMap |> Map.tryFind hashTag with
                 | None -> "<span style='color: red'>#" + hashTag + "</span>"
                 | Some title -> "<a href='../" + hashTag + "'>" + title + "</a>"

               let pattern = "{{#" + hashTag + "#}}"
               contents |> String.replace pattern link)
             page.Contents

      { page with Contents = contents })

  let indexPage =
    let markdown =
      inputPages
      |> List.sortByDescending (fun (page: Page) -> page.Slug)
      |> List.map (fun (page: Page) -> sprintf "- [%s](./%s)\n" page.Title page.Slug)
      |> String.concat ""

    let htmlContents =
      let _, _, markdownContents = Markdown.parse markdown
      Markdown.toHtml markdownContents

    let page: Page =
      { Title = "My Site"
        Slug = ""
        HashTags = []
        Contents = htmlContents }

    page

  let pages = indexPage :: inputPages

  let outputFiles =
    pages
    |> List.map (fun (page: Page) ->
      let outputFile =
        if page.Slug = "" then
          basedPath outputDir "index.html"
        else
          basedPath outputDir (page.Slug + "/index.html")

      let contents =
        let p: PageRenderParams =
          { Title = page.Title
            Contents = page.Contents }

        renderHtml p

      outputFile, contents)

  outputFiles
  |> List.map (fun (filename, _) -> dirname filename)
  |> set
  |> Set.iter createDir

  outputFiles
  |> List.iter (fun (filename, contents) -> writeFile filename contents)

[<EntryPoint>]
let main args =
  match args |> Array.toList with
  | []
  | "-h" :: _
  | "--help" :: _
  | "help" :: _ ->
    printf "%s" help
    exit 1

  | "-V" :: _
  | "--version" :: _
  | "version" :: _ ->
    printf "0.1.0\n"
    exit 1

  | "build" :: _ ->
    cmdBuild ()
    exit 0

  | arg :: _ ->
    printfn "Unknown argument '%s'." arg
    exit 1
