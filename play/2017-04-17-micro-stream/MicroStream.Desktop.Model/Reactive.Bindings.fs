namespace MicroStream.Reactive.Bindings

open Reactive.Bindings

type Behavior<'x> = ReactiveProperty<'x>

type IROBehavior<'x> = IReadOnlyReactiveProperty<'x>

type ObservableList<'x> = ReactiveCollection<'x>

module Behavior =
  let create value =
    new Behavior<_>(initialValue = value)

module ObservableList =
  let create () = new ObservableList<_>()
