open System

open Util.Control

[<EntryPoint>]
let main argv =
  
  let inc =
    update {
      let! x = State.get ()
      return! State.put (x + 1)
    }

  let logs =
    update {
      do! Writer.write 1
    }
    |> Writer.writeRun

  printfn "%A" 0

  // exit code
  0
