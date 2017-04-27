namespace MicroStream.StreamViews

open System
open System.Reactive.Disposables
open FSharp.Control.Reactive
open MicroStream
open MicroStream.Reactive.Bindings

/// Represents a view of stream (timeline).
type StreamView(stream: IObservable<Post>) =
  let disposables = new CompositeDisposable()

  let items = new ObservableList<StreamPost>()

  let addPost post =
    items.InsertOnScheduler(0, StreamPost(post))

  do
    stream
    |> Observable.performFinally
      (fun () ->
        disposables.Dispose()
      )
    |> Observable.subscribe addPost
    |> disposables.Add

  member this.Items = items

  member this.Dispose() =
    disposables.Dispose()

  interface IDisposable with
    override this.Dispose() =
      this.Dispose()

  static member val Empty =
    new StreamView(Observable.empty)
