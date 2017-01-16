namespace Tuktuk.Wpf.Controls

open Reactive.Bindings

type Page() =
  inherit TabPage()

  let name = "page"

  let ancestorList = AncestorList("path/to/directory")

  let fileTree = new FileTree()

  let fileCollection = FileCollection()

  member this.Name =
    name

  member this.AncestorList =
    ancestorList

  member this.FileTree =
    fileTree

  member this.FileCollection =
    fileCollection

  override this.TabHeader =
    this.Name :> obj
