namespace Zelga.Core

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Specialized
open System.Reactive.Subjects

type Timeline<'TValue>(toTime: 'TValue -> DateTimeOffset) =
  /// Items in ascending order by time.
  let list =
    List<'TValue>()

  let lowerBound value =
    let time = value |> toTime
    let rec loop lb ub =
      if ub - lb <= 1 then
        ub
      else
        let m = lb + (ub - lb) / 2
        if time < (list.[m] |> toTime) then
          loop lb m
        else
          loop m ub
    loop -1 list.Count

  let onAdded =
    new Subject<'TValue>()

  let collectionChangedEvent =
    Event<NotifyCollectionChangedEventHandler, NotifyCollectionChangedEventArgs>()

  let collectionChangedEventHandler =
    collectionChangedEvent.Publish

  let trigger createArgs this =
    collectionChangedEvent.Trigger(this, createArgs NotifyCollectionChangedAction.Add)

  member this.Add(value: 'TValue) =
    let index = lowerBound value
    list.Insert(index, value)
    this |> trigger (fun a -> NotifyCollectionChangedEventArgs(a, value :> obj, index))
    onAdded.OnNext(value)

  interface IObservable<'TValue> with
    override this.Subscribe(observer) =
      onAdded.Subscribe(observer)

  interface IReadOnlyList<'TValue> with
    override this.Count = list.Count

    override this.Item
      with get i = list.[i]

    override this.GetEnumerator() = list.GetEnumerator() :> IEnumerator<'TValue>

    override this.GetEnumerator(): IEnumerator = list.GetEnumerator() :> IEnumerator

  interface INotifyCollectionChanged with
    [<CLIEvent>]
    override this.CollectionChanged = collectionChangedEvent.Publish
