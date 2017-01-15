namespace Tuktuk.Wpf.Controls

open Reactive.Bindings

type Book() =
  inherit TabPage()

  override this.TabHeader =
    "book" :> obj
