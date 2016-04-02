namespace Util.Control

[<AutoOpen>]
module StateMonad =
  open Basis.Core

  type StateUpdateType<'s> = 
    | StateUpdate of option<'s>

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
    let put s  = Update (fun _ -> (StateUpdate (Some s), ()))
    let get () = Update (fun (s: 's) -> (StateUpdate (None: option<StateUpdateType<'s>>), s))

    let eval s m =
      Update.run m s |> snd

  let inc =
    State.get () |> Update.bind (fun x ->
      State.put (x + 1))
