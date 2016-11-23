namespace AsterSql.Core

module Dictionary =
  open System.Collections.Generic

  let toMap this =
    this
    |> Seq.map (fun (KeyValue (key, value)) -> (key, value))
    |> Map.ofSeq

open System.Collections.Generic

type Long = int64

type ROList<'x> = IReadOnlyList<'x>
