namespace Zelga.Core.Utility

[<AutoOpen>]
module Misc =
  let tap f x = f x; x

module Option =
  let getOr x =
    function
    | Some x -> x
    | None -> x

  let retry count fail f =
    let rec loop i =
      if i >= count then
        fail ()
      else
        try
          match f () with
          | Some x -> x
          | None -> loop (i + 1)
        with
        | _ -> loop (i + 1)
    loop 0

module String =
  open System

  let isNullOrEmpty s =
    String.IsNullOrEmpty(s)

  let trim (s: string) =
    s.Trim()

  let shorten (s: string) =
    if s.Length > 40 then
      s.Substring(0, 30) + "..."
    else
      s

module Async =
  let constant x =
    async {
      return x
    }

  let map f a =
    async {
      let! x = a
      return f x
    }

module Diagnostics =
  open System.Diagnostics
  open System.Text

  let start (verb, arg) =
    let psi =
      ProcessStartInfo
        ( FileName = verb
        , Arguments = arg
        , CreateNoWindow = true
        , UseShellExecute = false
        , RedirectStandardOutput = true
        , StandardOutputEncoding = Encoding.UTF8
        )
    Process.Start(psi)

  let tryExecuteParallel timeout commands =
    async {
      let stopwatch = Stopwatch()
      let processes = commands |> Seq.map start
      stopwatch.Start()
      let! result =
        processes
        |> Seq.map
          (fun p ->
            if p.WaitForExit(timeout - (int stopwatch.ElapsedMilliseconds)) then
              p.StandardOutput.ReadToEndAsync()
              |> Async.AwaitTask
              |> Async.map Some
            else Async.constant None
          )
        |> Async.Parallel
      for p in processes do
        p.Dispose()
      return result
    }

  let tryExecute timeout command =
    use p = start command
    if p.WaitForExit(timeout)
    then Some (p.StandardOutput.ReadToEnd())
    else None

module Observable =
  open System

  let subscribeAll
    (onNext: 'x -> unit)
    (onError: exn -> unit)
    (onCompleted: unit -> unit)
    (this: IObservable<'x>)
    =
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
