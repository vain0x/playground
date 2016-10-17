namespace Zelga.Core.Utility

module ReactiveProperty =
  open Reactive.Bindings

  let Create(value: 'x): ReactiveProperty<'x> =
    new ReactiveProperty<'x>
      ( value
      , ReactivePropertyMode.DistinctUntilChanged ||| ReactivePropertyMode.RaiseLatestValueOnSubscribe
      )

module ObservableCollection =
  open System.Collections.ObjectModel

  let Empty<'x> = ObservableCollection<'x>()

  let OfSeq (xs: seq<'x>) =
    ObservableCollection(xs)
