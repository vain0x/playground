namespace DotNetKit.FSharp.ErrorHandling

[<RequireQualifiedAccess>]
module Result =
  let isOk (result: Result<_, _>): bool =
    match result with
    | Ok _ ->
      true
    | Error _ ->
      false

  let isError (result: Result<_, _>): bool =
    result |> isOk |> not

  let tryGet (result: Result<'x, _>): option<'x> =
    match result with
    | Ok x ->
      Some x
    | Error _ ->
      None

  let tryGetError (result: Result<_, 'e>): option<'e> =
    match result with
    | Ok _ ->
      None
    | Error e ->
      Some e
      
  let getOr (value: 'x) (result: Result<'x, _>): 'x =
    match result with
    | Ok x ->
      x
    | Error _ ->
      value

  let getErrorOr (error: 'e) (result: Result<_, 'e>): 'e =
    match result with
    | Ok _ ->
      error
    | Error e ->
      e

  let getOrElse (getValue: unit -> 'x) (result: Result<'x, _>): 'x =
    match result with
    | Ok x ->
      x
    | Error _ ->
      getValue ()

  let getErrorOrElse (getError: unit -> 'e) (result: Result<_, 'e>): 'e =
    match result with
    | Ok _ ->
      getError ()
    | Error e ->
      e

  let getOrThrow (result: Result<'x, _>): 'x =
    match result with
    | Ok x ->
      x
    | Error _ ->
      invalidOp "Result doesn't have a value."

  let getErrorOrThrow (result: Result<_, 'e>): 'e =
    match result with
    | Ok _ ->
      invalidOp "Result doesn't have an error."
    | Error e ->
      e

  let flatten (result: Result<Result<'x, 'e>, 'e>): Result<'x, 'e> =
    match result with
    | Ok (Ok x) ->
      Ok x
    | Ok (Error e) ->
      Error e
    | Error e ->
      Error e

  let flattenError (result: Result<'x, Result<'x, 'e>>): Result<'x, 'e> =
    match result with
    | Ok x ->
      Ok x
    | Error (Ok x) ->
      Ok x
    | Error (Error e) ->
      Error e

  let map (f: 'x -> 'y) (result: Result<'x, 'e>): Result<'y, 'e> =
    match result with
    | Ok x ->
      Ok (f x)
    | Error e ->
      Error e

  let mapError (f: 'e -> 'f) (result: Result<'x, 'e>): Result<'x, 'f> =
    match result with
    | Ok x ->
      Ok x
    | Error e ->
      Error (f e)
      
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

  let tryApply<'x, 'y, 'e when 'e :> exn> (f: 'x -> 'y) (x: 'x): Result<'y, 'e> =
    try
      f x |> Ok
    with
    | :? 'e as e ->
      Error e

namespace DotNetKit.FSharp.ComputationExpression
  open DotNetKit.FSharp.ErrorHandling
  
  [<Sealed>]
  type ResultBuilder internal () =
    member this.Return(x) =
      Ok x

    member this.ReturnFrom(result: Result<_, _>) =
      result

    member this.Bind(m, f) =
      m |> Result.bind f

    member this.Using(x, f) =
      using x f

  [<Sealed>]
  type ResultErrorBuilder internal () =
    member this.Return(x) =
      Error x

    member this.ReturnFrom(result: Result<_, _>) =
      result

    member this.Bind(m, f) =
      m |> Result.bindError f

    member this.Using(x, f) =
      using x f

namespace DotNetKit.FSharp.ErrorHandling
  open DotNetKit.FSharp.ComputationExpression

  [<AutoOpen>]
  module ResultSyntax =
    let result = ResultBuilder()

    let resultError = ResultErrorBuilder()
