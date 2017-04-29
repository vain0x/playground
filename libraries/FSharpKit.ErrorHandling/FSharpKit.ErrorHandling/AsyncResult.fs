namespace FSharpKit.ErrorHandling

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module AsyncResult =
  module Internal =
    /// Creates an asynchronous workflow,
    /// which owns the specified resource and disposes it on finally.
    let inline using (x: 'x) (f: 'x -> Async<'y>): Async<'y> =
      async {
        use x = x
        return! f x
      }

  type AsyncResultFullBuilder internal () =
    member inline this.Run(f) = f ()

    member inline this.Delay(f): unit -> Async<Result<'x, 'e>> = f

    member inline this.Return(x): Async<Result<'x, 'e>> =
      async {
        return Ok x
      }

    member inline this.ReturnFrom(ar): Async<Result<'x, 'e>> =
      ar

    member inline this.Zero(): Async<Result<unit, 'e>> =
      async {
        return Ok ()
      }

    member this.Bind(ar, f): Async<Result<'y, 'e>> =
      async {
        let! r = ar
        match r with
        | Ok x ->
          return! f x
        | Error e ->
          return Error e
      }

    member this.Bind(r, f): Async<Result<'y, 'e>> =
      async {
        match r with
        | Ok x ->
          return! f x
        | Error e ->
          return Error e
      }

    member inline this.Bind(a: Async<unit>, f): Async<Result<'x, 'e>> =
      async {
        do! a
        return! f ()
      }

    member inline this.Using(x, f): Async<Result<'y, 'e>> =
      Internal.using x f

    member inline this.TryWith(f, h): Async<Result<'x, 'e>> =
      async {
        try
          return! f ()
        with
        | e ->
          return! h e
      }

    member inline this.TryFinally(f, onFinally): Async<Result<'x, 'e>> =
      async {
        try
          return! f ()
        finally
          onFinally ()
      }

    member this.Combine(ar, continuation): Async<Result<'x, 'e>> =
      async {
        let! r = ar
        match r with
        | Ok () ->
          return! continuation ()
        | Error e ->
          return Error e
      }

    member this.While(guard, f): Async<Result<unit, 'e>> =
      let rec loop () =
        async {
          if guard () then
            let! r = f ()
            match r with
            | Ok () ->
              return! loop ()
            | Error e ->
              return Error e
          else
            return Ok ()
        }
      loop ()

    member this.For(xs: seq<'x>, f): Async<Result<unit, 'e>> =
      async {
        use enumerator = xs.GetEnumerator()
        let rec loop () =
          async {
            if enumerator.MoveNext() then
              let! r = f enumerator.Current
              match r with
              | Ok () ->
                return! loop ()
              | Error e ->
                return Error e
            else
              return Ok ()
          }
        return! loop ()
      }

  /// Builds an asynchronous workflow which may be terminated with an error
  /// using computation expression syntax.
  let build = AsyncResultFullBuilder()

  /// Creates a failable asynchronous workflow,
  /// which executes synchronously and just returns the specified value as the result value.
  let ok (x: 'x): Async<Result<'x, 'e>> =
    async {
      return Ok x
    }

  /// Creates a failable asynchronous workflow,
  /// which executes synchronously and is immediately terminated with the specified error.
  let error (e: 'e): Async<Result<'x, 'e>> =
    async {
      return Error e
    }

  /// Creates a failable asynchronous workflow,
  /// which always results in a successful value.
  let ofAsync (a: Async<'x>): Async<Result<'x, 'e>> =
    async {
      let! x = a
      return Ok x
    }

  /// Creates a failable asynchronous workflow,
  /// which executes synchronously and just returns the specified result.
  let ofResult (r: Result<'x, 'e>): Async<Result<'x, 'e>> =
    async {
      return r
    }

  /// Converts a failable asynchronous workflow
  /// by mapping the result value with the specified function.
  let map (f: 'x -> 'y) (ar: Async<Result<'x, 'e>>): Async<Result<'y, 'e>> =
    async {
      let! r = ar
      return r |> Result.map f
    }

  /// Binds.
  let bind f (ar: Async<Result<'x, 'e>>): Async<Result<'y, 'e>> =
    build {
      let! x = ar
      return! f x
    }
