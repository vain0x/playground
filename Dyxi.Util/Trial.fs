namespace Dyxi.Util

open Printf
open Chessie.ErrorHandling

module Trial =
  let inject x =
    trial { return x }

  let ignore self =
    self |> Trial.lift (konst ())

  let uncons =
    function
    | Pass x              -> (Some x, None)
    | Warn (x, msgs)      -> (Some x, Some msgs) 
    | Fail msgs           -> (None, Some msgs)

  let warnf x fmt =
    kprintf (fun msg -> warn msg x) fmt

  let failf fmt =
    kprintf fail fmt

  let runConsoleApp (result: Result<unit, string>): int =
    let printMessages msgs =
      for msg in msgs do eprintfn "%s" (string msg)
    match result with
    | Pass () ->
        0
    | Warn ((), msgs) ->
        eprintfn "WARNING"
        printMessages msgs
        0
    | Fail msgs ->
        eprintfn "ERROR"
        printMessages msgs
        1
