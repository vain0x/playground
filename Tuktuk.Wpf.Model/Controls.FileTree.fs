namespace Tuktuk.Wpf.Controls

open Reactive.Bindings

type FileTreeNode(name: string) =
  do ()

  member this.Name =
    name

[<Sealed>]
type FileTree() =
  member this.Roots =
    [| FileTreeNode("root") |]
