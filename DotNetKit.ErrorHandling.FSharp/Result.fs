namespace DotNetKit.ErrorHandling.FSharp

open DotNetKit
open DotNetKit.ErrorHandling
open DotNetKit.FSharp.ErrorHandling

[<RequireQualifiedAccess>]
module Result =
  let ofFSharpResult<'x, 'e> : Result<'x, 'e> -> ErrorHandling.Result<'x, 'e> =
    function
    | Success x ->
      Result.Success<'x, 'e>(x)
    | Failure e ->
      Result.Failure<'x, 'e>(e)

  let toFSharpResult<'x, 'e> (result: ErrorHandling.Result<'x, 'e>): Result<'x, 'e> =
    result.Match(Success, Failure)
