namespace Tuktuk.Wpf.Controls

open System
open System.Reactive.Linq
open DotNetKit.FSharp
open Reactive.Bindings
open Tuktuk.Reactive.Bindings

[<Sealed>]
type Workspace(page: Page) =
  let page = page |> ReactiveProperty.create

  member this.AncestorList =
    page.Value.AncestorList

  member this.
