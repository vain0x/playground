namespace Util.Control

open System
open System.Collections.Generic

[<AutoOpen>]
module Operators =
  let inline empty< ^m when ^m: (static member Empty: unit -> ^m)> () =
    (^m: (static member Empty: unit -> ^m) ())
  
  let inline append< ^m when ^m: (static member Append: ^m * ^m -> ^m)> l r =
    (^m: (static member Append: ^m * ^m -> ^m) (l, r))

  /// a.k.a ``return``, pure, singlton
  let inline result< ^m, ^t when ^m: (static member Return: ^t -> ^m)>
      (x: ^t)
      : ^m
    = (^m: (static member Return: ^t -> ^m) (x))

  let inline bind< ^m_t, ^m_u, ^t when ^m_t: (static member Bind: ^m_t * (^t -> ^m_u) -> ^m_u)>
      (f    : ^t -> ^m_u)
      (x    : ^m_t)
      : ^m_u
    = (^m_t: (static member Bind: ^m_t * (^t -> ^m_u) -> ^m_u) (x, f))

  let inline (>>=) x f = bind f x

type FlowControl =
  | Break
  | Continue

[<RequireQualifiedAccess>]
module Builders =
  type Base() =
    member this.Delay(f: unit -> _ * FlowControl) =
      f

    member this.Run(f: unit -> _ * FlowControl) =
      f () |> fst

    member this.ReturnFrom(x) =
      (x, Break)

    member this.Combine
        ( (x, flow)   : 't * FlowControl
        , kont        : unit -> 't * FlowControl
        )             : 't * FlowControl
      =
      match flow with
      | Break -> (x, Break)
      | Continue -> kont ()

    member this.Using
        ( x     : #IDisposable
        , f     : #IDisposable -> 't * FlowControl
        )       : 't * FlowControl
      =
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

  let while'
      combine'    // b.Combine
      zero'       // b.Zero
      ( guard   : unit -> bool
      , f       : unit -> _ )
      : _ * FlowControl
    =
    let rec loop () =
      if guard () then
        let x = f ()
        in combine' (x, loop)
      else zero' ()
    in loop ()

  let for'
      using'      // b.Using
      while'      // b.While
      ( xs      : #seq<'t>
      , f        )
                : _ * FlowControl
    =
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

[<AutoOpen>]
module ContMonad =
  type Cont<'r, 't> =
    | Cont of (('t -> 'r) -> 'r)

  [<RequireQualifiedAccess>]
  module Cont =
    let run (Cont x) = x

    let callCC
        (f    : ('t -> Cont<'r, _>) -> Cont<'r, 't>)
              : Cont<'r, 't>
      =
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
    member inline this.Return(x: 't): Update<'s, 'u, 't> * FlowControl =
      (Update.result x, Break)

    member inline this.Bind
        ( x       : Update<'s, 'u, 'x>
        , f       : 'x -> Update<'s, 'u, 'y> * FlowControl
        )         : Update<'s, 'u, 'y> * FlowControl
      =
      (Update.bind (f >> fst) x, Continue)

    member inline this.Zero() =
      this.Return(())

    member inline this.Run(f)     = f () |> fst
    member inline this.Delay(f)   = f

    member inline this.Combine((x, flow), kont) =
      match flow with
      | Break -> (x, Break)
      | Continue -> kont ()

    member inline this.ReturnFrom(m) = (m, Break)

    member inline this.Using(x: #IDisposable, f: _ -> _ * FlowControl) =
      try f x
      finally
        match box x with
        | null -> ()
        | notNull -> x.Dispose()

    member inline this.While(guard, f) =
      Builders.while' this.Combine this.Zero (guard, f)

    member inline this.For(xs: #seq<_>, f) =
      Builders.for' this.Using this.While (xs, f)

  let update = UpdateBuilder()

[<AutoOpen>]
module ReaderMonad =
  type ReaderUpdate<'s> =
    | NoUpdate
  with
    static member Empty()                     = NoUpdate
    static member Append(NoUpdate, NoUpdate)  = NoUpdate
    static member Update(s, NoUpdate)         = s

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
    static member Empty()               = Log []
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
  open Basis.Core
  open Util.Collections

  type StateUpdate<'t> = 
    | StateUpdate of option<'t>

    static member Empty() = StateUpdate None

    static member Append(StateUpdate l, StateUpdate r) =
      Option.union r l |> StateUpdate

    static member Update(s, StateUpdate u) = 
      u |> Option.getOr s

  [<RequireQualifiedAccess>]
  module State =
    let put s  = Update (fun _ -> (StateUpdate (Some s), ()))
    let get () = Update (fun s -> (StateUpdate None, s))

    let eval s m =
      Update.run m s |> snd

    let inline exec s m =
      eval s (Update.bind (fun _ -> get ()) m)
