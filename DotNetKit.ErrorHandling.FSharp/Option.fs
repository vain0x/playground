namespace DotNetKit.ErrorHandling.FSharp

open DotNetKit.ErrorHandling

module Option =
  let ofFSharpOption<'x> =
    function
    | Some x ->
      Option.Some(x)
    | None ->
      Option<'x>.None

  let toFSharpOption (this: Option<'x>) =
    this.Match(Some, fun () -> None)
