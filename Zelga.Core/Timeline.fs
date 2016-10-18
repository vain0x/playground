namespace Zelga.Core

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Specialized
open System.Linq
open System.Reactive.Subjects

type Timeline<'TValue>(toTime: 'TValue -> DateTimeOffset) =
  /// Items sorted by time in ascending order.
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

  let collectionChangedEvent =
    Event<NotifyCollectionChangedEventHandler, NotifyCollectionChangedEventArgs>()

  let collectionChangedEventHandler =
    collectionChangedEvent.Publish

  let trigger createArgs this =
    collectionChangedEvent.Trigger(this, createArgs NotifyCollectionChangedAction.Add)

  member this.Add(value: 'TValue) =
    let index = lowerBound value
    list.Insert(index, value)
    this |> trigger (fun a -> NotifyCollectionChangedEventArgs(a, value :> obj, list.Count - index))

  member this.GetEnumerator() =
    Enumerable.Reverse(list).GetEnumerator()

  interface IReadOnlyList<'TValue> with
    override this.Count = list.Count

    override this.Item
      with get i = list.[list.Count - 1 - i]

    override this.GetEnumerator() = this.GetEnumerator()

    override this.GetEnumerator() = this.GetEnumerator() :> IEnumerator

  interface INotifyCollectionChanged with
    [<CLIEvent>]
    override this.CollectionChanged = collectionChangedEvent.Publish
