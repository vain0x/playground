namespace Tuktuk.Wpf.Controls

open System
open System.Collections.Generic
open System.IO
open Reactive.Bindings
open SharpFileSystem
open SharpFileSystem.FileSystems
open Tuktuk.Reactive.Bindings

type IFileTreeNode =
  inherit IDisposable

  abstract Children: IReadOnlyReactiveProperty<IReadOnlyList<IFileTreeNode>>
  abstract IsExpanded: ReactiveProperty<bool>

[<Sealed>]
type VoidFileTreeNode() =
  static let children =
    ReactiveProperty.create ([||] :> IReadOnlyList<_>) :> IReadOnlyReactiveProperty<_>

  let isExpanded = ReactiveProperty.create false

  static member val Instance =
    new VoidFileTreeNode()

  static member val Instances =
    [| VoidFileTreeNode.Instance :> IFileTreeNode |] :> IReadOnlyList<_>

  interface IFileTreeNode with
    override this.Children =
      children

    override this.IsExpanded =
      isExpanded

  interface IDisposable with
    override this.Dispose() =
      ()

[<Sealed>]
type DirectoryFileTreeNode(fileSystem: IFileSystem, path: FileSystemPath, name) =
  let isExpanded = ReactiveProperty.create false

  let fetchEntities () =
    try
      let items = fileSystem.GetEntities(path)
      (items.Count <> 0, items :> seq<_>)
    with
    | _ ->
      (false, Seq.empty)

  let fetchChildren isExpanded =
    match fetchEntities () with
    | (false, _) ->
      VoidFileTreeNode.Instances
    | (true, entities) ->
      entities
      |> Seq.choose
        (fun entry ->
          if entry.IsDirectory then
            let path = path.AppendDirectory(entry.EntityName)
            new DirectoryFileTreeNode(fileSystem, path, path.EntityName) :> IFileTreeNode
            |> Some
          else
            None
        )
      |> Seq.toArray
      :> IReadOnlyList<_>

  let children =
    isExpanded |> ReactiveProperty.map
      (function
        | true -> fetchChildren ()
        | false -> Array.empty :> IReadOnlyList<_>
      )

  member this.Name =
    name

  member this.Children =
    children

  member this.Dispose() =
    fileSystem.Dispose()

  interface IFileTreeNode with
    override this.Children =
      children :> IReadOnlyReactiveProperty<_>

    override this.IsExpanded =
      isExpanded

  interface IDisposable with
    override this.Dispose() =
      this.Dispose()

[<Sealed>]
type FileTree(roots) =
  let selectedItem =
    roots |> Seq.tryHead |> ReactiveProperty.create

  new() =
    let roots =
      match DriveInfo.GetDrives() with
      | [||] ->
        VoidFileTreeNode.Instances
      | drives ->
        drives |> Array.map
          (fun drive ->
            let directory = drive.RootDirectory
            let fileSystem = new PhysicalFileSystem(directory.FullName)
            let path = FileSystemPath.Root
            new DirectoryFileTreeNode(fileSystem, path, drive.Name) :> IFileTreeNode
          )
        :> IReadOnlyList<_>
    new FileTree(roots)

  member this.Roots =
    roots :> IReadOnlyList<_>

  member this.SelectedItem =
    selectedItem

  member this.OnSelected(selectedItemOrNull: obj) =
    match selectedItemOrNull with
    | null
    | :? VoidFileTreeNode ->
      selectedItem.Value <- None
    | :? DirectoryFileTreeNode as node ->
      selectedItem.Value <- Some (node :> _)
    | _ ->
      InvalidOperationException() |> raise

  member this.Dispose() =
    roots |> Seq.iter (fun root -> root.Dispose())

  interface IDisposable with
    override this.Dispose() =
      this.Dispose()
