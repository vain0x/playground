
[<AutoOpen>]
module Util =
  module String =
    let replace (source: string) destination (self: string) =
      self.Replace(source, destination)

    let replaceBy (map: Map<string, string>) (self: string) =
      map |> Map.fold (fun self k v -> self |> replace k v) self

  module File =
    open System.IO

    let mapAsString f path =
      let content = File.ReadAllText(path)
      let content' = content |> f
      if content <> content' then
        File.WriteAllText(path, content')

module Program =
  open System
  open System.IO

  let defaultGuids =
    [
      "F184B08F-C81C-45F6-A57F-5ABD9991F28F"
      "5F1923B2-C8BB-4A0E-9F7F-25D9DDBB1A92"
      "36757C03-1694-486D-B93B-E8FE94979DCA"
      "8a823414-0fc0-4d53-a0de-2403f7bc0740"
      "5c50d068-e747-4444-8114-955d070e827c"
    ]

  let defaultSolutionName = "dot_net_lab"
  let defaultProjectName = "DotNetLab.Vb.Lib"

  let solutionFileName solutionName = solutionName + ".sln"
  let testProjectName projectName = projectName + ".Test"
  let projectFileName projectName = projectName + ".vbproj"
  let projectFilePath projectName = Path.Combine(projectName, projectFileName projectName)

  let rename solutionName projectName =
    Environment.CurrentDirectory <- "../../../"

    // Rename solution.
    File.Move(solutionFileName defaultSolutionName, solutionFileName solutionName)

    // Rename project.
    let replaceContent =
      let map =
        seq {
          yield (defaultProjectName, projectName)
          for guid in defaultGuids do
            yield (guid, Guid.NewGuid() |> string)
        } |> Map.ofSeq
      in
        File.mapAsString (String.replaceBy map)

    let renameProject oldName newName =
      File.Move
        ( projectFilePath oldName
        , Path.Combine(oldName, projectFileName newName)
        )
      Directory.Move(oldName, newName)

      seq {
        yield projectFilePath newName
        yield! Directory.GetFiles(Path.Combine(newName, "My Project"))
      }
      |> Seq.iter replaceContent

    solutionFileName solutionName |> replaceContent
    renameProject defaultProjectName projectName
    renameProject (testProjectName defaultProjectName) (testProjectName projectName)

  let run () =
    printfn "%s" "Solution name?: "
    match Console.ReadLine() with
    | null -> ()
    | solutionName ->
      printfn "%s" "First project name?: "
      match Console.ReadLine() with
      | null -> ()
      | projectName ->
        rename solutionName projectName

Program.run ()
