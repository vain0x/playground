namespace FileSystemDotNet.Core

module Directory =
  /// Enumerates ancestors of the directory, excluding itself.
  let ancestors: IDirectory -> list<IDirectory> =
    let rec loop acc (dir: IDirectory) =
      match dir.Parent with
      | None -> acc
      | Some parent -> loop (parent :: acc) parent
    in loop []

  let getAllFilesIfAble (dir: IDirectory) =
    try
      dir.GetFiles()
    with
    | _ -> [||]

  let getAllDirectoriesIfAble (dir: IDirectory) =
    try
      dir.GetDirectories()
    with
    | _ -> [||]

  let tryFindFile name dir =
    dir |> getAllFilesIfAble |> Array.tryFind (fun file -> file.Name = name)

  let tryFindDirectory name dir =
    dir |> getAllDirectoriesIfAble |> Array.tryFind (fun dir -> dir.Name = name)
