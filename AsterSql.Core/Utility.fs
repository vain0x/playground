namespace AsterSql.Core

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Misc =
  open System

  let todo () =
    NotImplementedException() |> raise

  let never () =
    InvalidOperationException() |> raise

module Dictionary =
  open System.Collections.Generic

  let toMap this =
    this
    |> Seq.map (fun (KeyValue (key, value)) -> (key, value))
    |> Map.ofSeq

open System.Collections
open System.Collections.Generic

type Long = int64

type ROList<'x> = IReadOnlyList<'x>
