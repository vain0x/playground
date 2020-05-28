namespace FSharpKit.ErrorHandling

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Option =
  type OptionMinimalBuilder internal () =
    member inline this.Return(x): Option<'x> =
      Some x

    member inline this.ReturnFrom(option): Option<'x> =
      option

    member inline this.Zero(): Option<unit> =
      Some ()

    member inline this.Bind(o, f): Option<'x> =
      match o with
      | Some x ->
        f x
      | None ->
        None

    member inline this.Using(x, f): Option<'x> =
      using x f

  type OptionFullBuilder internal () =
    inherit OptionMinimalBuilder()

    member this.Run(f): Option<'x> = f ()

    member this.Delay(f): unit -> Option<'x> = f

    member this.TryWith(f, h): Option<'x> =
      try
        f ()
      with
      | e -> h e

    member this.TryFinally(f, g): Option<'x> =
      try
        f ()
      finally
        g ()

    member this.Combine(o, f): Option<'x> =
      match o with
      | Some () ->
        f ()
      | None ->
        None

    member this.While(guard, f): Option<unit> =
      let rec loop () =
        if guard () then
          this.Combine(f (), loop)
        else
          Some ()
      loop ()

    member this.For(xs: seq<'x>, f): Option<unit> =
      use enumerator = xs.GetEnumerator()
      let rec loop () =
        if enumerator.MoveNext() then
          this.Combine(f enumerator.Current, loop)
        else
          Some ()
      loop ()

  /// Builds a computation which be may terminated with an error
  /// using computation expression syntax.
  /// Supports minimal syntax for performance.
  let build' = OptionMinimalBuilder()

  /// Builds a computation which be may terminated with an error
  /// using computation expression syntax.
  /// Supports full syntax.
  let build = OptionFullBuilder()
