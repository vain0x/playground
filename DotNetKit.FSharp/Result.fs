namespace DotNetKit.FSharp.ErrorHandling

[<RequireQualifiedAccess>]
module Result =
  let isSuccess (result: Result<_, _>): bool =
    match result with
    | Success _ ->
      true
    | Failure _ ->
      false

  let isFailure (result: Result<_, _>): bool =
    result |> isSuccess |> not

  let tryGet (result: Result<'x, _>): option<'x> =
    match result with
    | Success x ->
      Some x
    | Failure _ ->
      None

  let tryGetError (result: Result<_, 'e>): option<'e> =
    match result with
    | Success _ ->
      None
    | Failure e ->
      Some e
      
  let getOr (value: 'x) (result: Result<'x, _>): 'x =
    match result with
    | Success x ->
      x
    | Failure _ ->
      value

  let getErrorOr (error: 'e) (result: Result<_, 'e>): 'e =
    match result with
    | Success _ ->
      error
    | Failure e ->
      e

  let getOrElse (getValue: unit -> 'x) (result: Result<'x, _>): 'x =
    match result with
    | Success x ->
      x
    | Failure _ ->
      getValue ()

  let getErrorOrElse (getError: unit -> 'e) (result: Result<_, 'e>): 'e =
    match result with
    | Success _ ->
      getError ()
    | Failure e ->
      e

  let getOrThrow (result: Result<'x, _>): 'x =
    match result with
    | Success x ->
      x
    | Failure _ ->
      invalidOp "Result doesn't have a value."

  let getErrorOrThrow (result: Result<_, 'e>): 'e =
    match result with
    | Success _ ->
      invalidOp "Result doesn't have an error."
    | Failure e ->
      e

  let flatten (result: Result<Result<'x, 'e>, 'e>): Result<'x, 'e> =
    match result with
    | Success (Success x) ->
      Success x
    | Success (Failure e) ->
      Failure e
    | Failure e ->
      Failure e

  let flattenError (result: Result<'x, Result<'x, 'e>>): Result<'x, 'e> =
    match result with
    | Success x ->
      Success x
    | Failure (Success x) ->
      Success x
    | Failure (Failure e) ->
      Failure e

  let map (f: 'x -> 'y) (result: Result<'x, 'e>): Result<'y, 'e> =
    match result with
    | Success x ->
      Success (f x)
    | Failure e ->
      Failure e

  let mapError (f: 'e -> 'f) (result: Result<'x, 'e>): Result<'x, 'f> =
    match result with
    | Success x ->
      Success x
    | Failure e ->
      Failure (f e)
      
  let bind (f: 'x -> Result<'y, 'e>) (result: Result<'x, 'e>): Result<'y, 'e> =
    result |> map f |> flatten

  let bindError (f: 'e -> Result<'x, 'f>) (result: Result<'x, 'e>): Result<'x, 'f> =
    result |> mapError f |> flattenError

  let exists (p: 'x -> bool) (result: Result<'x, _>): bool =
    result |> tryGet |> Option.exists p

  let existsError (p: 'e -> bool) (result: Result<_, 'e>): bool =
    result |> tryGetError |> Option.exists p

  let forall (p: 'x -> bool) (result: Result<'x, _>): bool =
    result |> tryGet |> Option.forall p

  let forallError (p: 'e -> bool) (result: Result<_, 'e>): bool =
    result |> tryGetError |> Option.forall p
