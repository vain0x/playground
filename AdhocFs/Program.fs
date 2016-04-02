open System

open Util.Control

[<EntryPoint>]
let main argv =
  
  let inc =
    update {
      let! x = State.get ()
      do! State.put (x + 1)
    }

  let x =
    update {
      for i in 1..10 do
        do! inc
      return! State.get ()
    }
    |> State.eval 0

  printfn "%A" x

  // exit code
  0
