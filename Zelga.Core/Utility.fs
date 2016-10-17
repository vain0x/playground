namespace Zelga.Core.Utility

[<AutoOpen>]
module Misc =
  let tap f x = f x; x

module Option =
  let getOr x =
    function
    | Some x -> x
    | None -> x

module Diagnostics =
  open System.Diagnostics
  open System.Text

  let execCmdline timeout verb arg =
    let psi =
      ProcessStartInfo
        ( FileName = verb
        , Arguments = arg
        , CreateNoWindow = true
        , UseShellExecute = false
        , RedirectStandardOutput = true
        , StandardOutputEncoding = Encoding.UTF8
        )
    use p = Process.Start(psi)
    if p.WaitForExit(timeout)
    then Some (p.StandardOutput.ReadToEnd())
    else None

module Observable =
  open System

  let subscribeAll (onNext: _ -> unit) (onError: exn -> unit) (onCompleted: unit -> unit) (this: IObservable<_>) =
    this.Subscribe(onNext, onError, onCompleted)

  let subscribeCompleted onCompleted this =
    this |> subscribeAll ignore ignore onCompleted

module ReactiveProperty =
  open Reactive.Bindings

  let Create(value: 'x): ReactiveProperty<'x> =
    new ReactiveProperty<'x>
      ( value
      , ReactivePropertyMode.DistinctUntilChanged ||| ReactivePropertyMode.RaiseLatestValueOnSubscribe
      )

module ObservableCollection =
  open System.Collections.ObjectModel
  open System.Collections.Specialized
  open System.Linq
  open System.Reactive.Linq

  let Empty<'x> () = ObservableCollection<'x>()

  let OfSeq (xs: seq<'x>) =
    ObservableCollection(xs)

  let ObserveCount (this: ObservableCollection<_>) =
    ReactiveProperty.Create this.Count
    |> tap
      (fun count ->
        let subscription =
          this.CollectionChanged |> Observable.subscribe (fun p -> count.Value <- this.Count)
        count |> Observable.subscribeCompleted subscription.Dispose |> ignore
      )

  let ObserveAdded (this: ObservableCollection<'x>) =
    this.CollectionChanged
      .Where(fun e -> e.Action = NotifyCollectionChangedAction.Add)
      .SelectMany(fun e -> e.NewItems.Cast<'x>().ToObservable())
