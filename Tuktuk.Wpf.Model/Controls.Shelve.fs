namespace Tuktuk.Wpf.Controls

open System
open System.Reactive.Linq
open DotNetKit.FSharp
open Reactive.Bindings
open Tuktuk.Reactive.Bindings

type Shelve() =
  let books =
    [| Book("book0"); Book("book1") |]
    |> ReactiveCollection.ofSeq

  let selectedBook =
    books.[0] |> ReactiveProperty.create

  let pages =
    selectedBook |> ReactiveProperty.map (fun book -> book.Pages)
    
  let workspaces =
    [|
      ReactiveProperty.create pages.Value.[0]
      ReactiveProperty.create pages.Value.[0]
    |]

  let activeWorkspace =
    workspaces.[0] |> ReactiveProperty.create

  let appTitle =
    activeWorkspace
    |> ReactiveProperty.bind (fun page -> page.Value.Name)
    |> ReactiveProperty.map (fun name -> sprintf "%s - Tuktuk" name)
    :> IReadOnlyReactiveProperty<_>

  member this.Books =
    books

  member this.Pages =
    pages
    
  member this.Workspace0 =
    workspaces.[0]

  member this.Workspace1 =
    workspaces.[1]

  member this.AppTitle =
    appTitle
