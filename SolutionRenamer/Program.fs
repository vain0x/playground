namespace SolutionRenamer

module Model =
  open System
  open System.IO

  let initialState =
    {
      Map             = Map.empty
      Extensions      = Constants.extensions
      IgnoreList      = Constants.ignoreList
    }

  let addReplace source destination (state: RenamerState) =
    { state with
        Map = state.Map |> Map.add source destination
    }

  let replace (state: RenamerState) text =
    text |> String.replaceBy state.Map

  let subfiles state (directory: DirectoryInfo) =
    seq {
      for subfile in directory.GetFiles() do
        if state.IgnoreList |> Set.contains subfile.Name |> not
          && state.Extensions |> Set.contains subfile.Extension
        then
          yield subfile
    }

  let subdirectories state (directory: DirectoryInfo) =
    seq {
      for subdir in directory.GetDirectories() do
        if state.IgnoreList |> Set.contains subdir.Name |> not then
          yield subdir
    }

  let collectGuids (directory: DirectoryInfo) state =
    let findGuids file =
      Constants.guidRegex
      |> Regex.matchAll (file |> FileInfo.readAllText)
      |> Seq.map (fun m -> m.Value)
    let rec walk state (source: DirectoryInfo) =
      seq {
        for subfile in source |> subfiles state do
          yield findGuids subfile |> Set.ofSeq
        for subdir in source |> subdirectories state do
          yield! subdir |> walk state
      }
    let guids =
      directory
      |> walk state
      |> Set.unionMany
    let map =
      guids
      |> Seq.map (fun guid -> (guid, Guid.NewGuid().ToString()))
      |> Map.ofSeq
    in
      { state with Map = Map.merge state.Map map }

  let renameImpl (directory: DirectoryInfo) state =
    let rec walk state (source: DirectoryInfo) =
      // Replace name and content of subfiles.
      for subfile in source |> subfiles state do
        let newFileName = subfile.Name |> replace state
        // Replace content.
        subfile.FullName |> File.mapAsString (replace state)
        // Replace name.
        if subfile.Name <> newFileName then
          subfile.MoveTo(Path.Combine(source.FullName, newFileName))

      // Replace name of subdirectories.
      for subdir in source |> subdirectories state do
        let newDirectoryName = subdir.Name |> replace state
        // Do recursively.
        subdir |> walk state
        // Replace name.
        if subdir.Name <> newDirectoryName then
          subdir.MoveTo(Path.Combine(source.FullName, newDirectoryName))
    in
      directory |> walk state

  let rename directory state =
    state
    |> collectGuids directory
    |> renameImpl directory

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
        initialState
        |> addReplace settings.SourceSolutionName solutionName 
        |> addReplace settings.SourceProjectName projectName
        |> Model.rename settings.SolutionDirectory

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
#if DEBUG
    | [] ->
      run
        {
          SourceSolutionName            = "dot_net_lab"
          SourceProjectName             = "DotNetLab.Cs.Lib"
          SolutionDirectory             = DirectoryInfo(".")
        }
#endif
    | _ ->
      printfn "SolutionRenamer.exe solution_directory"

    // exit code
    0
