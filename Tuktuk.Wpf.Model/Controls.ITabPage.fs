namespace Tuktuk.Wpf.Controls

open Reactive.Bindings

type ITabPage =
  abstract TabHeader: IReadOnlyReactiveProperty
