namespace DotNetKit.ErrorHandling.FSharp

open DotNetKit.ErrorHandling

[<RequireQualifiedAccess>]
module Option =
  let toCSharpOption<'x> =
    function
    | Some x ->
      Option.Some(x)
    | None ->
      Option<'x>.None

  let ofCSharpOption (this: Option<'x>) =
    this.Match(Some, fun () -> None)
