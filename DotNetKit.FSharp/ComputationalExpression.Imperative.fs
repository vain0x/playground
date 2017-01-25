namespace DotNetKit.FSharp.ComputationalExpression
  open System
  open DotNetKit.FSharp.ErrorHandling

  /// Represents an intermediate result of imperative operations.
  /// None means that it continues to calculate or returns "void";
  /// Some x means that the total computation resulted in x.
  type private ImperativeResult<'x> =
    option<'x>

  [<Sealed>]
  type ImperativeBuilder internal () =
    member this.Delay(f: unit -> ImperativeResult<'x>) =
      f

    member this.Run(f: unit -> ImperativeResult<'x>) =
      match f () with
      | None ->
        // Never come because of the conversion rule.
        InvalidOperationException() |> raise
      | Some x ->
        x

    member this.Run(f: unit -> ImperativeResult<unit>) =
      f () |> ignore

    member this.Return(x) =
      Some x

    member this.Combine(r: ImperativeResult<'x>, f: unit -> ImperativeResult<'x>) =
      match r with
      | None ->
        f ()
      | Some x ->
        Some x

    member this.Using(x: 'x, f: 'x -> ImperativeResult<'y>) =
      using x f

    member this.TryWith(f: unit -> 'x, h: exn -> 'x) =
      Default.tryWith f h

    member this.TryFinally(f: unit -> 'x, g: unit -> unit) =
      Default.tryFinally f g

    member this.Zero() =
      None

    member this.While(p, f) =
      Default.``while`` this.Combine this.Zero p f

    member this.For(xs, f) =
      Default.``for`` this.Using this.While xs f

namespace DotNetKit.FSharp
  [<AutoOpen>]
  module ImperativeSyntax =
    let imperative =
      ComputationalExpression.ImperativeBuilder()
