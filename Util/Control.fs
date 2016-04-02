namespace Util.Control

module Operators =
  let inline empty< ^m when ^m: (static member Empty: ^m)> () =
    (^m: (static member Empty: ^m) ())
  
  let inline append< ^m when ^m: (static member Append: ^m * ^m -> ^m)> l r =
    (^m: (static member Append: ^m * ^m -> ^m) (l, r))

[<AutoOpen>]
module UpdateMonad =
  open System.Collections.Generic
  open Operators

  type Update<'s, 'u, 't> =
    | Update of ('s -> 'u * 't)

  [<RequireQualifiedAccess>]
  module Update =
    let run (Update x) = x

    let inline update< ^u, ^s when ^u: (static member Update: ^s * ^u -> ^s)> s u =
      (^u: (static member Update: ^s * ^u -> ^s) (s, u))

    let inline get () = Update (fun s -> (empty (), s))
    let inline up u   = Update (fun _ -> (u, ()))

    let inline bind f m =
      Update (fun s ->
        let (u1, x) = run m s
        let (u2, y) = run (f x) (update s u1)
        in (append u1 u2, y)
        )

    let inline result x =
      Update (fun _ -> (empty (), x))

  type UpdateBuilder internal () =
    member inline this.Return(x) =
      Update.result x

    member inline this.Bind(x, f) =
      Update.bind f x

    member inline this.Zero() =
      this.Return(())

    member inline this.Run(f) = f ()

    member inline this.Delay(f) =
      (fun () -> this.Bind(this.Zero(), f))

    member inline this.Combine(c1, c2) =
      this.Bind(c1, c2)

    member inline this.ReturnFrom(m) = m

    member inline this.Using(r,f) =
      Update (fun s -> 
        use rr = r
        in Update.run (f rr) s
        )

    member inline this.While(guard, f) =
      let rec loop () =
        if guard ()
        then this.Combine(f (), loop)
        else this.Zero()
      in loop ()

    member inline this.For(xs: #seq<_>, f) =
      this.Using
        ( xs.GetEnumerator()
        , (fun (iter: IEnumerator<_>) ->
            this.While(iter.MoveNext, (fun () -> f (iter.Current))))
        )

  let update = UpdateBuilder()

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

    let put s  = Update (fun _ -> (StateUpdate (Some s), ()))
    let get () = Update (fun s -> (StateUpdate None, run s))

    let eval s m =
      Update.run m (State s) |> snd
