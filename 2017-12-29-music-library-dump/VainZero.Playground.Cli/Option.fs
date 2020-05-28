namespace VainZero.FSharpErrorHandling

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Option =
  type OptionMinimalBuilder internal () =
    member inline __.Return(x): Option<'x> =
      Some x

    member inline __.ReturnFrom(option): Option<'x> =
      option

    member inline __.Zero(): Option<unit> =
      Some ()

    member inline __.Bind(o, f): Option<'x> =
      match o with
      | Some x ->
        f x
      | None ->
        None

    member inline __.Using(x, f): Option<'x> =
      using x f

  type OptionFullBuilder internal () =
    inherit OptionMinimalBuilder()

    member __.Run(f): Option<'x> = f ()

    member __.Delay(f): unit -> Option<'x> = f

    member __.TryWith(f, h): Option<'x> =
      try
        f ()
      with
      | e -> h e

    member __.TryFinally(f, g): Option<'x> =
      try
        f ()
      finally
        g ()

    member __.Combine(o, f): Option<'x> =
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
