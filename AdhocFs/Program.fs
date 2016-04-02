open System

open Util.Control

[<AutoOpen>]
module StateMonad =
  open Basis.Core

  type StateState<'t> =
    | State of 't

  type StateUpdate<'t> = 
    | StateUpdate of option<'t>

    static member Empty = StateUpdate None

    static member Append(StateUpdate l, StateUpdate r) = 
      match l, r with 
      | None, x
      | x, None -> x |> StateUpdate
      | Some _, Some s -> Some s |> StateUpdate

    static member Update(s, StateUpdate u) = 
      u |> Option.getOr s

  [<RequireQualifiedAccess>]
  module State =
    let run (State s) = s

    let put s  = Update (fun _ -> (StateUpdate (Some (State s)), ()))
    let get () = Update (fun s -> (StateUpdate None, run s))

    let eval s m =
      Update.run m (State s) |> snd

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
