open System

open Util.Control

[<EntryPoint>]
let main argv =

  let inc =
    State.get () |> Update.bind (fun x ->
      State.put (x + 1))

  printfn "%A" 0

  // exit code
  0
