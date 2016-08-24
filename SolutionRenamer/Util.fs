namespace SolutionRenamer

[<AutoOpen>]
module Misc =
  let fold xs f s =
    xs |> Seq.fold (fun s x -> f x s) s

module Map =
  let merge l r =
    l |> fold r (fun (KeyValue (k, v)) map -> map |> Map.add k v)
 
module String =
  let replace (source: string) destination (self: string) =
    self.Replace(source, destination)

  let replaceBy (map: Map<string, string>) (self: string) =
    map |> Map.fold (fun self k v -> self |> replace k v) self

module Regex =
  open System.Linq
  open System.Text.RegularExpressions

  let matchAll input (regex: Regex) =
    regex.Matches(input).Cast<Match>()

module File =
  open System.IO

  let mapAsString f path =
    let content = File.ReadAllText(path)
    let content' = content |> f
    if content <> content' then
      File.WriteAllText(path, content')

module FileInfo =
  open System.IO

  let readAllText (file: FileInfo) =
    File.ReadAllText(file.FullName)

module Directory =
  open System.IO
  
  let moveTo r l =
    if Directory.Exists(l) && Directory.Exists(r) then
      if Path.GetPathRoot(l) <> Path.GetPathRoot(r) then
        invalidOp "Can't move directory to different root."
      let t = Path.Combine(Path.GetPathRoot(l), Path.GetRandomFileName())
      Directory.Delete(r, (* recursive = *) true)
      Directory.Move(l, r)

  let swap l r =
    if Directory.Exists(l) && Directory.Exists(r) then
      if Path.GetPathRoot(l) <> Path.GetPathRoot(r) then
        invalidOp "Can't swap directories under different roots."
      let t = Path.Combine(Path.GetPathRoot(l), Path.GetRandomFileName())
      Directory.Move(l, t)
      Directory.Move(r, l)
      Directory.Move(t, r)

module DirectoryInfo =
  open System.IO

  let deleteIfExists (self: DirectoryInfo) =
    if Directory.Exists(self.FullName) then
      self.Delete((* recursive = *) true)
