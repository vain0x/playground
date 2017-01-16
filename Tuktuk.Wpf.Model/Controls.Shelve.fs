namespace Tuktuk.Wpf.Controls

open System
open System.Reactive.Disposables
open System.Reactive.Linq
open DotNetKit.FSharp
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

  let selectedBookAndPage =
    activeWorkspace |> ReactiveProperty.map
      (fun workspace ->
        books |> Seq.tryPick
          (fun book ->
            book.Pages |> Seq.tryPick
              (fun page ->
                if page = workspace.Page.Value
                then Some (book, page)
                else None
              ))
        |> Option.get // NOTE: Each page is owned by any of books.
      )
    |> tap disposables.Add

  let selectedBook =
    selectedBookAndPage |> ReactiveProperty.map fst
    |> tap disposables.Add

  let pages =
    selectedBook |> ReactiveProperty.map (fun book -> book.Pages)
    |> tap disposables.Add

  let selectedPage =
    selectedBookAndPage |> ReactiveProperty.map snd
    |> tap disposables.Add

  let appTitle =
    activeWorkspace
    |> ReactiveProperty.bind (fun workspace -> workspace.Page.Value.Name :> _)
    |> ReactiveProperty.map (fun name -> sprintf "%s - Tuktuk" name)
    :> IReadOnlyReactiveProperty<_>
    |> tap disposables.Add

  do
    selectedBook.Pairwise() |> Observable.subscribe
      (fun (OldNewPair (oldBook, newBook)) ->
        let workspace = activeWorkspace.Value
        oldBook.ActivePage.Value <- workspace.Page.Value
        workspace.Page.Value <- newBook.ActivePage.Value
      )
    |> disposables.Add

  new(fileSystem, books, openPages) =
    let workspaces =
      openPages |> Array.map (fun page -> Workspace(page))
    new Shelve(fileSystem, books, workspaces)

  member this.Books =
    books

  member this.SelectedBook =
    selectedBook

  member this.Pages =
    pages

  member this.SelectedPage =
    selectedPage

  member this.ActiveWorkspace =
    activeWorkspace

  member this.Workspace0 =
    workspaces.[0]

  member this.Workspace1 =
    workspaces.[1]

  member this.AppTitle =
    appTitle
