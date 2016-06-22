namespace FileSystemDotNet.Core

open System.IO

module File =
  let fullName (file: IFileBase) =
    let ancestors =
      file.Parent
      |> Option.map (fun dir ->
        (dir |> Directory.ancestors) @ [dir]
        |> List.map (fun dir -> dir :> IFileBase)
        )
      |> (function | Some a -> a | None -> [])
    let names =
      (ancestors |> List.map (fun file -> file.Name)) @ [file.Name]
      |> List.toArray
    in Path.Combine(names)
