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

[<Sealed>]
type Workspace
  ( fileSystem: IFileSystem
  , selectedBook: ReactiveProperty<Book>
  , pages: ReactiveProperty<ReactiveCollection<Page>>
  , selectedPage: ReactiveProperty<Page>
  , disposables: CompositeDisposable
  ) =
  let gotFocus =
    new Subject<unit>()
    |> tap disposables.Add

  let dispose () =
    disposables.Dispose()

  do
    selectedPage |> Observable.subscribe
      (fun page ->
        selectedBook.Value.ActivePage.Value <- page
      )
    |> disposables.Add

  new(fileSystem, book: Book, page: Page) =
    let disposables =
      new CompositeDisposable()

    let selectedBook =
      ReactiveProperty.create book
      |> tap disposables.Add

    let pages =
      selectedBook
      |> ReactiveProperty.map (fun book -> book.Pages)
      |> tap disposables.Add

    let selectedPage =
      pages
      |> ReactiveProperty.map (fun _ -> selectedBook.Value.ActivePage.Value)
      |> tap disposables.Add

    new Workspace(fileSystem, selectedBook, pages, selectedPage, disposables)

  member this.SelectedBook =
    selectedBook

  member this.Pages =
    pages

  member this.SelectedPage =
    selectedPage

  member this.GotFocus =
    gotFocus

  member this.Dispose() =
    dispose ()

  interface IDisposable with
    override this.Dispose() =
      this.Dispose()
