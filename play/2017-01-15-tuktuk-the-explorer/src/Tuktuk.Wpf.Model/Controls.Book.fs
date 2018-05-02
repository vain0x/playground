namespace Tuktuk.Wpf.Controls

open System
open System.IO
open System.Reactive.Disposables
open System.Reactive.Linq
open System.Runtime.Serialization
open DotNetKit.FSharp
open Reactive.Bindings
open SharpFileSystem
open Tuktuk.Reactive.Bindings
open Tuktuk.Runtime.Serialization

[<Sealed>]
type Book
  ( fileSystem: IFileSystem
  , name: string
  , pages: seq<Page>
  ) =
  let disposables =
    new CompositeDisposable()

  let name =
    name |> ReactiveProperty.create
    |> tap disposables.Add

  let pages =
    pages |> ReactiveCollection.ofSeq
    |> tap disposables.Add

  let activePage =
    pages.[0] |> ReactiveProperty.create
    |> tap disposables.Add

  let dispose () =
    pages |> Seq.iter (fun page -> page.Dispose())
    disposables.Dispose()

  member this.AddPage(directoryPath: FileSystemPath) =
    let page = new Page(fileSystem, directoryPath)
    pages.AddOnScheduler(page)

  member this.Pages =
    pages

  member this.ActivePage =
    activePage

  member this.Dispose() =
    dispose ()

  interface IDisposable with
    override this.Dispose() =
      this.Dispose()

  interface ITabPage with
    override this.TabHeader =
      name :> IReadOnlyReactiveProperty
