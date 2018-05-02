namespace Tuktuk.Wpf.Controls

open Reactive.Bindings

[<Sealed>]
type AncestorList(pathString: string) =
  let pathString = new ReactiveProperty<_>(initialValue = pathString)

  member this.PathString = pathString
