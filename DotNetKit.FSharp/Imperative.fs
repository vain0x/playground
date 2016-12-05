namespace DotNetKit.FSharp.ComputationExpression
  open System
  open DotNetKit.FSharp.ErrorHandling

  /// Represents an intermediate result of imperative operations.
  /// Ok () means that it continues to calculate;
  /// Error x means that the total computation resulted in x.
  type private ImperativeResult<'x> =
    Result<unit, 'x>

  [<Sealed>]
  type ImperativeBuilder internal () =
    member this.Delay(f: unit -> ImperativeResult<'x>) =
      f

    member this.Run(f: unit -> ImperativeResult<'x>) =
      match f () with
      | Ok () ->
        // Never come because of the conversion rule.
        InvalidOperationException() |> raise
      | Error x ->
        x

    member this.Run(f: unit -> ImperativeResult<unit>) =
      f () |> ignore

    member this.Return(x) =
      Error x

    member this.Combine(r: ImperativeResult<'x>, f: unit -> ImperativeResult<'x>) =
      match r with
      | Ok () ->
        f ()
      | Error x ->
        Error x

    member this.Using(x: 'x, f: 'x -> ImperativeResult<'y>) =
      using x f

    member this.TryWith(f: unit -> 'x, h: exn -> 'x) =
      Default.tryWith f h

    member this.TryFinally(f: unit -> 'x, g: unit -> unit) =
      Default.tryFinally f g

    member this.Zero() =
      Ok ()

    member this.While(p, f) =
      Default.``while`` this.Combine this.Zero p f

    member this.For(xs, f) =
      Default.``for`` this.Using this.While xs f

namespace DotNetKit.FSharp
  [<AutoOpen>]
  module ImperativeSyntax =
    let imperative =
      ComputationExpression.ImperativeBuilder()
