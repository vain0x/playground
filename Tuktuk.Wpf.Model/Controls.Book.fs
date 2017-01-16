namespace Tuktuk.Wpf.Controls

open System
open System.IO
open System.Reactive.Linq
open System.Runtime.Serialization
open DotNetKit.FSharp
open Reactive.Bindings
open SharpFileSystem
open Tuktuk.Reactive.Bindings
open Tuktuk.Runtime.Serialization

[<Sealed>]
type Book(fileSystem: IFileSystem, name: string, pages: seq<Page>) =
  let name =
    name |> ReactiveProperty.create

  let pages =
    pages |> ReactiveCollection.ofSeq

  member this.AddPage(directoryPath: FileSystemPath) =
    let page = new Page(fileSystem, directoryPath)
    pages.AddOnScheduler(page)

  member this.Pages =
    pages

  interface ITabPage with
    override this.TabHeader =
      name :> IReadOnlyReactiveProperty
