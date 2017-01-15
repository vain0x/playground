namespace Tuktuk.Wpf.Controls

open System
open System.Collections.Generic
open System.IO
open System.Reactive.Concurrency
open System.Reactive.Linq
open Reactive.Bindings
open SharpFileSystem
open SharpFileSystem.FileSystems
open Tuktuk.Reactive.Bindings

type IFileTreeNode =
  inherit IDisposable

  abstract Children: IReadOnlyReactiveProperty<IReadOnlyList<IFileTreeNode>>
  abstract IsExpanded: ReactiveProperty<bool>

[<Sealed>]
type SpinnerFileTreeNode() =
  static let children =
    ReactiveProperty.create ([||] :> IReadOnlyList<_>) :> IReadOnlyReactiveProperty<_>

  let isExpanded = ReactiveProperty.create false

  static member val Instance =
    new SpinnerFileTreeNode()

  static member val Instances =
    [| SpinnerFileTreeNode.Instance :> IFileTreeNode |] :> IReadOnlyList<_>

  interface IFileTreeNode with
    override this.Children =
      children

    override this.IsExpanded =
      isExpanded

  interface IDisposable with
    override this.Dispose() =
      ()

[<Sealed>]
type DirectoryFileTreeNode
  ( fileSystem: IFileSystem
  , path: FileSystemPath
  , name: string
  , fetches: IReadOnlyReactiveProperty<bool>
  ) =
  let isExpanded = ReactiveProperty.create false

  let fetchChildrenAsync =
    async {
      let subpaths =
        try
          (fileSystem: IFileSystem).GetEntities((path: FileSystemPath))
          |> Seq.filter (fun p -> p.IsDirectory)
          |> Seq.toArray
        with
        | _ ->
          Array.empty
      let nodes =
        subpaths |> Array.map
          (fun subpath ->
            new DirectoryFileTreeNode
              ( fileSystem
              , subpath
              , subpath.EntityName
              , isExpanded
              ) :> IFileTreeNode
          )
      return nodes :> IReadOnlyList<_>
    }

  static let emptyChildren =
    ReactiveProperty.create (Array.empty :> IReadOnlyList<_>)
    :> IReadOnlyReactiveProperty<_>

  let children =
    fetches |> ReactiveProperty.bind
      (function
        | true ->
          fetchChildrenAsync |> ReactiveProperty.ofAsync SpinnerFileTreeNode.Instances
        | false ->
          emptyChildren
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
      let drives = DriveInfo.GetDrives()
      let nodes =
        drives |> Array.map
          (fun drive ->
            let directory = drive.RootDirectory
            let fileSystem = new PhysicalFileSystem(directory.FullName)
            let path = FileSystemPath.Root
            let node =
              new DirectoryFileTreeNode
                ( fileSystem
                , path
                , drive.Name
                , ReactiveProperty.``true``
                )
            node :> IFileTreeNode
          )
      nodes :> IReadOnlyList<_>
    new FileTree(roots)

  member this.Roots =
    roots :> IReadOnlyList<_>

  member this.SelectedItem =
    selectedItem

  member this.OnSelected(selectedItemOrNull: obj) =
    match selectedItemOrNull with
    | null
    | :? SpinnerFileTreeNode ->
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
