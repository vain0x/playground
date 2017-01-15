namespace Tuktuk.Wpf.Controls

open System
open System.Reactive.Linq
open System.Reactive.Subjects
open DotNetKit.FSharp
open Reactive.Bindings
open Tuktuk.Reactive.Bindings

[<Sealed>]
type Workspace(page: Page) =
  let page = page |> ReactiveProperty.create

  let gotFocus = new Subject<unit>()

  member this.Page =
    page :> IReadOnlyReactiveProperty<_>

  member this.GotFocus =
    gotFocus
