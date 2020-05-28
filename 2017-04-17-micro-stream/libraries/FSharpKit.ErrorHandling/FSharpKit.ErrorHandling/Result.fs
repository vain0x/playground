namespace FSharpKit.ErrorHandling

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Result =
  /// Gets a value indicating whether the result has a successful value.
  let isOk (result: Result<'x, 'e>): bool =
    match result with
    | Ok _ ->
      true
    | Error _ ->
      false

  /// Gets a value indicating whether the result has an error.
  let isError (result: Result<'x, 'e>): bool =
    match result with
    | Ok _ ->
      false
    | Error _ ->
      true

  /// Tries to get the successful value from the result.
  let tryValue (result: Result<'x, 'e>): option<'x> =
    match result with
    | Ok x ->
      Some x
    | Error _ ->
      None

  /// Tries to get the error from the result.
  let tryError (result: Result<'x, 'e>): option<'e> =
    match result with
    | Ok _ ->
      None
    | Error e ->
      Some e

  /// Gets the successful value from the result if able.
  /// Returns the specified value otherwise.
  let defaultValue (value: 'x) (result: Result<'x, 'e>): 'x =
    match result with
    | Ok x ->
      x
    | Error _ ->
      value

  /// Gets the error from the result if able.
  /// Returns the specified error otherwise.
  let defaultError (error: 'e) (result: Result<'x, 'e>): 'e =
    match result with
    | Ok _ ->
      error
    | Error e ->
      e

  /// Gets the successful value from the result if able.
  /// Invokes the specified function otherwise.
  let defaultWith (getValue: unit -> 'x) (result: Result<'x, 'e>): 'x =
    match result with
    | Ok x ->
      x
    | Error _ ->
      getValue ()

  /// Gets the error from the result if able.
  /// Invokes the specified function otherwise.
  let defaultWithError (getError: unit -> 'e) (result: Result<'x, 'e>): 'e =
    match result with
    | Ok _ ->
      getError ()
    | Error e ->
      e

  /// Gets the successful value from the result.
  /// Raises an InvalidOperationException otherwise.
  let valueOrRaise (result: Result<'x, _>): 'x =
    match result with
    | Ok x ->
      x
    | Error _ ->
      invalidOp "Result has no value."

  /// Gets the error from the result.
  /// Raises an InvalidOperationException otherwise.
  let errorOrRaise (result: Result<_, 'e>): 'e =
    match result with
    | Ok _ ->
      invalidOp "Result has no error."
    | Error e ->
      e

  /// Flattens the result with a nested value type.
  let flatten (result: Result<Result<'x, 'e>, 'e>): Result<'x, 'e> =
    match result with
    | Ok (Ok x) ->
      Ok x
    | Ok (Error e) ->
      Error e
    | Error e ->
      Error e

  /// Flattens the result with a nested error type.
  let flattenError (result: Result<'x, Result<'x, 'e>>): Result<'x, 'e> =
    match result with
    | Ok x ->
      Ok x
    | Error (Ok x) ->
      Ok x
    | Error (Error e) ->
      Error e

  /// Maps the error of the result with the specified function if it has an error.
  /// Returns the given result otherwise.
  let bindError (f: 'e -> Result<'x, 'f>) (result: Result<'x, 'e>): Result<'x, 'f> =
    match result with
    | Ok x ->
      Ok x
    | Error e ->
      f e

  /// If the result has a successful value, invokes the specified function.
  let iter (f: 'x -> unit) (result: Result<'x, 'e>): unit =
    match result with
    | Ok x ->
      f x
    | Error _ ->
      ()

  /// If the result has an error, invokes the specified function.
  let iterError (f: 'e -> unit) (result: Result<'x, 'e>): unit =
    match result with
    | Ok _ ->
      ()
    | Error e ->
      f e

  /// Gets a value indicating whether the result has a successful value which satisfies the predicate.
  let exists (p: 'x -> bool) (result: Result<'x, 'e>): bool =
    match result with
    | Ok x ->
      p x
    | Error _ ->
      false

  /// Gets a value indicating whether the result has an error which satisfies the predicate.
  let existsError (p: 'e -> bool) (result: Result<'x, 'e>): bool =
    match result with
    | Ok x ->
      false
    | Error e ->
      p e

  /// Gets a value indicating whether
  /// if the result has a successful value then it satisfies the predicate.
  let forall (p: 'x -> bool) (result: Result<'x, 'e>): bool =
    match result with
    | Ok x ->
      p x
    | Error _ ->
      true

  /// Gets a value indicating whether
  /// if the result has an error then it satisfies the predicate.
  let forallError (p: 'e -> bool) (result: Result<'x, 'e>): bool =
    match result with
    | Ok x ->
      true
    | Error e ->
      p e

  type ResultMinimalBuilder internal () =
    member inline this.Return(x) =
      Ok x

    member inline this.ReturnFrom(result: Result<_, _>) =
      result

    member inline this.Zero() =
      Ok ()

    member inline this.Bind(m, f) =
      m |> Result.bind f

    member inline this.Using(x, f) =
      using x f

  type ResultFullBuilder internal () =
    inherit ResultMinimalBuilder()

    member this.Run(f): Result<'x, 'e> = f ()

    member this.Delay(f): unit -> Result<'x, 'e> = f

    member this.TryWith(f, h): Result<'x, 'e> =
      try
        f ()
      with
      | e -> h e

    member this.TryFinally(f, g): Result<'x, 'e> =
      try
        f ()
      finally
        g ()

    member this.Combine(r, f): Result<'x, 'e> =
      match r with
      | Ok () ->
        f ()
      | Error e ->
        Error e

    member this.While(guard, f): Result<unit, 'e> =
      let rec loop () =
        if guard () then
          this.Combine(f (), loop)
        else
          Ok ()
      loop ()

    member this.For(xs: seq<'x>, f): Result<unit, 'e> =
      use enumerator = xs.GetEnumerator()
      let rec loop () =
        if enumerator.MoveNext() then
          this.Combine(f enumerator.Current, loop)
        else
          Ok ()
      loop ()

  /// Builds a computation which be may terminated with an error
  /// using computation expression syntax.
  /// Supports minimal syntax for performance.
  let build' = ResultMinimalBuilder()

  /// Builds a computation which be may terminated with an error
  /// using computation expression syntax.
  /// Supports full syntax.
  let build = ResultFullBuilder()

  type ResultErrorMinimalBuilder internal () =
    member inline this.Return(x) =
      Error x

    member inline this.ReturnFrom(result: Result<_, _>) =
      result

    member inline this.Zero() =
      Error ()

    member inline this.Bind(m, f) =
      m |> bindError f

    member inline this.Using(x, f) =
      using x f

  type ResultErrorFullBuilder internal () =
    inherit ResultErrorMinimalBuilder()

    member this.Run(f): Result<'x, 'e> = f ()

    member this.Delay(f): unit -> Result<'x, 'e> = f

    member this.TryWith(f, h): Result<'x, 'e> =
      try
        f ()
      with
      | e -> h e

    member this.TryFinally(f, g): Result<'x, 'e> =
      try
        f ()
      finally
        g ()

    member this.Combine(r, f): Result<'x, 'e> =
      match r with
      | Ok x ->
        Ok x
      | Error () ->
        f ()

    member this.While(guard, f): Result<'x, unit> =
      let rec loop () =
        if guard () then
          this.Combine(f (), loop)
        else
          Error ()
      loop ()

    member this.For(xs: seq<'x>, f): Result<'x, unit> =
      use enumerator = xs.GetEnumerator()
      let rec loop () =
        if enumerator.MoveNext() then
          this.Combine(f enumerator.Current, loop)
        else
          Error ()
      loop ()
      
  /// Builds a computation which may be terminated with a successful result value
  /// using computation expression syntax.
  /// Supports minimal syntax for performance.
  let buildError' = ResultErrorMinimalBuilder()

  /// Builds a computation which may be terminated with a successful result value
  /// using computation expression syntax.
  /// Supports full syntax.
  let buildError = ResultErrorFullBuilder()
