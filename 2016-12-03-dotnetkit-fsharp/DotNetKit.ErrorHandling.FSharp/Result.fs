namespace DotNetKit.ErrorHandling.FSharp

open DotNetKit
open DotNetKit.ErrorHandling
open DotNetKit.FSharp.ErrorHandling

[<RequireQualifiedAccess>]
module Result =
  let toCSharpResult<'x, 'e> : Result<'x, 'e> -> ErrorHandling.Result<'x, 'e> =
    function
    | Ok x ->
      Result.Success<'x, 'e>(x)
    | Error e ->
      Result.Failure<'x, 'e>(e)

  let ofCSharpResult<'x, 'e> (result: ErrorHandling.Result<'x, 'e>): Result<'x, 'e> =
    result.Match(Ok, Error)
