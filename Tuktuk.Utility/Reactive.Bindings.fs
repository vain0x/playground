namespace Tuktuk.Reactive.Bindings

open System
open System.Reactive.Linq
open System.Reactive.Threading.Tasks
open Reactive.Bindings

[<AutoOpen>]
module ReactiveProperty =
  let create initialValue =
    new ReactiveProperty<_>(initialValue = initialValue)

  let map f this =
    (this: IReadOnlyReactiveProperty<_>).Select(Func<_, _>(f)).ToReactiveProperty()

  let bind f this =
    (this: IReadOnlyReactiveProperty<_>)
      .Select(fun x -> (f(x): IReadOnlyReactiveProperty<_>) :> IObservable<_>)
      .Switch()
      .ToReactiveProperty()

  let ``true`` = true |> create :> IReadOnlyReactiveProperty<_>
  let ``false`` = false |> create :> IReadOnlyReactiveProperty<_>
  let emptyArray<'x> = Array.empty |> create :> IReadOnlyReactiveProperty<array<'x>>

  let ofAsync initialValue computation =
    (computation |> Async.StartAsTask)
      .ToObservable()
      .ToReadOnlyReactiveProperty(initialValue = initialValue)
    :> IReadOnlyReactiveProperty<_>
