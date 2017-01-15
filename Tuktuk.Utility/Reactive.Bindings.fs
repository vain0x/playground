namespace Tuktuk.Reactive.Bindings

open System
open System.Reactive.Linq
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
