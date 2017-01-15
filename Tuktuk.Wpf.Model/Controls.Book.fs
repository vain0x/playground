namespace Tuktuk.Wpf.Controls

open System
open System.IO
open System.Reactive.Linq
open DotNetKit.FSharp
open Reactive.Bindings
open Tuktuk.Reactive.Bindings

type Book(name) =
  let pages =
    [|
      Page.FromDirectory(DirectoryInfo(Environment.CurrentDirectory))
      Page.FromDirectory(DirectoryInfo(Environment.CurrentDirectory).Parent)
    |]
    |> ReactiveCollection.ofSeq

  member this.Pages =
    pages

  interface ITabPage with
    override this.TabHeader =
      name :> obj
