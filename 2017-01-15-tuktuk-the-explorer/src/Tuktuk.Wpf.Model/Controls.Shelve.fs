namespace Tuktuk.Wpf.Controls

open System
open System.Reactive.Disposables
open System.Reactive.Linq
open System.Reactive.Subjects
open DotNetKit.FSharp
open SharpFileSystem
open SharpFileSystem.FileSystems
open Reactive.Bindings
open Reactive.Bindings.Extensions
open SharpFileSystem
open Tuktuk.Reactive.Bindings

type Shelve
  ( fileSystem: IFileSystem
  , books: seq<Book>
  , workspaces: array<Workspace>
  ) =
  let disposables =
    new CompositeDisposable()

  let books =
    books |> ReactiveCollection.ofSeq
    |> tap disposables.Add

  let activeWorkspace =
    let gotFocus =
      workspaces |> Seq.map
        (fun workspace -> workspace.GotFocus.Select(fun _ -> workspace))
    gotFocus.Merge().ToReadOnlyReactiveProperty(workspaces.[0])
    :> IReadOnlyReactiveProperty<_>
    |> tap disposables.Add

  let appTitle =
    activeWorkspace
    |> ReactiveProperty.bind (fun workspace -> workspace.SelectedPage :> _)
    |> ReactiveProperty.bind (fun page -> page.Name :> _)
    |> ReactiveProperty.map (fun name -> sprintf "%s - Tuktuk" name)
    :> IReadOnlyReactiveProperty<_>
    |> tap disposables.Add

  member this.Books =
    books

  member this.ActiveWorkspace =
    activeWorkspace

  member this.Workspace0 =
    workspaces.[0]

  member this.Workspace1 =
    workspaces.[1]

  member this.AppTitle =
    appTitle
