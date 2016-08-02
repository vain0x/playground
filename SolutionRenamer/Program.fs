namespace SolutionRenamer

module Model =
  open System
  open System.IO

  let initialState =
    // Replace GUIDs to new GUIDs.
    let guidMap =
      Constants.guids
      |> List.map (fun guid -> (guid, Guid.NewGuid().ToString()))
      |> Map.ofList
    in
      {
        Map             = guidMap
        Extensions      = Constants.extensions
        IgnoreList      = Constants.ignoreList
      }

  let addReplace source destination (state: RenamerState) =
    { state with
        Map = state.Map |> Map.add source destination
    }

  let replace (state: RenamerState) text =
    text |> String.replaceBy state.Map

  let rec copyTo state (destination: DirectoryInfo) (source: DirectoryInfo) =
    // Copy subfiles, replacing their name and content.
    for subfile in source.GetFiles() do
      if state.IgnoreList |> Set.contains subfile.Name |> not then
        let newFileName = subfile.Name |> replace state
        let newFilePath =  Path.Combine(destination.FullName, newFileName)
        let newFile = subfile.CopyTo(newFilePath)
        // Replace content.
        if extensions |> Set.contains newFile.Extension then
          newFile.FullName |> File.mapAsString (replace state)

    // Copy subdirectories, replacing their name.
    for subdir in source.GetDirectories() do
      if state.IgnoreList |> Set.contains subdir.Name |> not then
        let newDirectoryName = subdir.Name |> replace state
        let newDirectoryPath = Path.Combine(destination.FullName, newDirectoryName)
        let newDirectory = Directory.CreateDirectory(newDirectoryPath)
        subdir |> copyTo state newDirectory

  let rename state (directory: DirectoryInfo) =
    let temporaryDirectory =
      DirectoryInfo(Path.Combine(directory.Root.FullName, directory.Name |> replace state))
    try
      temporaryDirectory.Create()
      directory |> copyTo state temporaryDirectory
      temporaryDirectory.FullName |> Directory.swap directory.FullName
    finally
      temporaryDirectory |> DirectoryInfo.deleteIfExists

module Program =
  open System
  open System.IO
  open Model

  type Settings =
    {
      SourceSolutionName        : string
      SourceProjectName         : string
      SolutionDirectory         : DirectoryInfo
    }

  let run (settings: Settings) =
    printfn "%s" "Solution name?: "
    match Console.ReadLine() with
    | null -> ()
    | solutionName ->
      printfn "%s" "First project name?: "
      match Console.ReadLine() with
      | null -> ()
      | projectName ->
        let state =
          initialState
          |> addReplace settings.SourceSolutionName solutionName 
          |> addReplace settings.SourceProjectName projectName
        settings.SolutionDirectory |> Model.rename state

  [<EntryPoint>]
  let main args =
    match args |> Array.toList with
    | [path; sourceSolutionName; sourceProjectName] when Directory.Exists(path) ->
      run
        {
          SourceSolutionName            = sourceSolutionName
          SourceProjectName             = sourceProjectName
          SolutionDirectory             = DirectoryInfo(path)
        }
    | _ ->
      printfn "SolutionRenamer.exe solution_directory"

    // exit code
    0
