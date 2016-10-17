namespace Zelga.Core

open System.Collections
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Collections.Specialized
open System.ComponentModel

type AddOnlyObservableList<'TValue>() as this =
  let list =
    ObservableCollection<'TValue>()

  let collectionChangedEvent =
    Event<NotifyCollectionChangedEventHandler, NotifyCollectionChangedEventArgs>()

  do
    list.CollectionChanged |> Event.add (fun args -> collectionChangedEvent.Trigger(this, args))

  member this.Add(value: 'TValue) =
    list.Add(value)

  interface IReadOnlyList<'TValue> with
    override this.Count = list.Count

    override this.Item
      with get i = list.[i]

    override this.GetEnumerator() = list.GetEnumerator()

    override this.GetEnumerator(): IEnumerator = list.GetEnumerator() :> IEnumerator

  interface INotifyCollectionChanged with
    [<CLIEvent>]
    override this.CollectionChanged = collectionChangedEvent.Publish
