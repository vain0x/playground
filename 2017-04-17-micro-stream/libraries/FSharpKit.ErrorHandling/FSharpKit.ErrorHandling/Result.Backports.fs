namespace Microsoft.FSharp.Core

// In F# 4.0 or lower, you need to define a macro named `LOWER_THAN_FSHARP_41`.

#if LOWER_THAN_FSHARP_41
/// Represents a result of computation:
/// a successful value in which the computation resulted
/// or an error with which terminated the computation.
type Result<'x, 'e> =
  | Ok of 'x
  | Error of 'e

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Result =
  /// Maps the successful value of the result if it has a value.
  /// Returns the specified result otherwise.
  let map (f: 'x -> 'y) (result: Result<'x, 'e>): Result<'y, 'e> =
    match result with
    | Ok x ->
      Ok (f x)
    | Error e ->
      Error e

  /// Maps the error of the result if it has an error.
  /// Returns the specified result otherwise.
  let mapError (f: 'e -> 'f) (result: Result<'x, 'e>): Result<'x, 'f> =
    match result with
    | Ok x ->
      Ok x
    | Error e ->
      Error (f e)

  /// Maps the successful value of the result if it has a value.
  /// Returns the specified result otherwise.
  let bind (f: 'x -> Result<'y, 'e>) (result: Result<'x, 'e>): Result<'y, 'e> =
    match result with
    | Ok x ->
      f x
    | Error e ->
      Error e
#endif
