namespace Util.Control

open System
open System.Collections.Generic
open Basis.Core

module Operators =
  let inline empty< ^m when ^m: (static member Empty: ^m)> () =
    (^m: (static member Empty: ^m) ())
  
  let inline append< ^m when ^m: (static member Append: ^m * ^m -> ^m)> l r =
    (^m: (static member Append: ^m * ^m -> ^m) (l, r))

type FlowControl =
  | Break
  | Continue

[<RequireQualifiedAccess>]
module Builders =
  open Operators
  
  type Base() =
    member this.Delay(f: unit -> _ * FlowControl) =
      f

    member this.Run(f: unit -> _ * FlowControl) =
      f () |> fst

    member this.ReturnFrom(x) =
      (x, Break)

    member this.Combine((x, flow), kont: unit -> _ * FlowControl) =
      match flow with
      | Break -> (x, Break)
      | Continue -> kont ()

    member this.Using(x: #IDisposable, f: _ -> _ * FlowControl) =
      try f x
      finally
        match box x with
        | null -> ()
        | notNull -> x.Dispose()

    member this.TryWith(f, h) =
      try f ()
      with e -> h e

    member this.TryFinally(f, g) =
      try f ()
      finally g ()

  let while' combine zero (guard, f) =
    let rec loop () =
      if guard () then
        let x = f ()
        in combine (x, loop)
      else zero ()
    in loop ()

  let for' using' while' (xs: #seq<_>, f) =
    using'
      ( xs.GetEnumerator()
      , (fun (iter: IEnumerator<_>) ->
          while' (iter.MoveNext, (fun () -> f (iter.Current))))
      )

[<AutoOpen>]
module IdentityMonad =
  type Identity<'t> =
    | Identity of 't

  [<RequireQualifiedAccess>]
  module Identity =
    let run (Identity x) = x

  type Identity<'t> with
    static member Return(x)     = Identity x
    static member Map(x, f)     = x |> Identity.run |> f |> Identity
    static member Bind(x, f)    = x |> Identity.run |> f

[<AutoOpen>]
module ContMonad =
  open Operators

  type Cont<'r, 't> =
    | Cont of (('t -> 'r) -> 'r)

  [<RequireQualifiedAccess>]
  module Cont =
    let run (Cont x) = x

    let callCC (f: ('t -> Cont<'r, _>) -> Cont<'r, 't>) =
      Cont (fun k ->
        k |> run (f (fun x -> Cont (fun _ -> k x)))
        )

    let result x = Cont (fun k -> k x)
    let bind f x = Cont (fun k -> run x (fun a -> run (f a) k))

  type ContBuilder internal () =
    inherit Builders.Base()

    member this.Zero()        = (Cont.result (), Continue)
    member this.Return(x)     = (Cont.result x, Break)
    member this.Bind(x, f)    = (Cont.bind (f >> fst) x, Continue)

    member this.While(guard, f) =
      Builders.while' this.Combine this.Zero (guard, f)
    member this.For(xs, f) =
      Builders.for' this.Using this.While (xs, f)

  let cont = ContBuilder()

[<AutoOpen>]
module UpdateMonad =
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
module ReaderMonad =
  type ReaderUpdate<'s> =
    | NoUpdate
  with
    static member Empty = NoUpdate
    static member Append(NoUpdate, NoUpdate) = NoUpdate
    static member Update(s, NoUpdate) = s

  [<RequireQualifiedAccess>]
  module Reader =
    let read x =
      Update (fun s -> (NoUpdate, s))

    let readRun s u =
      Update.run u s |> snd

[<AutoOpen>]
module WriterMonad =
  type WriterUpdate<'x> =
    internal
    | Log of list<'x>
  with
    static member Empty                 = Log []
    static member Append(Log l, Log r)  = List.append l r |> Log
    static member Update((), _)         = ()

  [<RequireQualifiedAccess>]
  module Writer =
    let write x =
      Update (fun _ -> (Log [x], ()))

    let writeRun u =
      let (Log us, v) = Update.run u ()
      in (v, us)

[<AutoOpen>]
module StateMonad =
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
