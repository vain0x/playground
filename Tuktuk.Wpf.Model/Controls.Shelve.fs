namespace Tuktuk.Wpf.Controls

open System.Reactive.Linq
open Reactive.Bindings
open Tuktuk.Reactive.Bindings

type Shelve() =
  let books =
    [| Book() |] |> ReactiveProperty.create

  let pages =
    [| Page() |] |> ReactiveProperty.create
    
  let openPage0 =
    ReactiveProperty.create pages.Value.[0]

  let openPage1 =
    ReactiveProperty.create pages.Value.[0]

  let selectedPage =
    ReactiveProperty.create pages.Value.[0]

  let appTitle =
    selectedPage
    |> ReactiveProperty.map (fun page -> sprintf "%s - Tuktuk" page.Name)
    :> IReadOnlyReactiveProperty<_>

  member this.Books =
    books

  member this.Pages =
    pages
    
  member this.OpenPage0 =
    openPage0

  member this.OpenPage1 =
    openPage0

  member this.AppTitle =
    appTitle
