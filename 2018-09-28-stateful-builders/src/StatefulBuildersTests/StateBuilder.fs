namespace StatefulBuilders

[<Struct>]
type ResultMutBuilder =
  member inline __.Run(f) = f ()

  member inline __.Delay(f) = f

  member inline __.Return(x) = (), x

  member inline __.ReturnFrom((s, x)) = s, x

  member inline __.Zero() = (), ()

  member inline __.Bind((s: 's, x: 'x), f: 'x -> 's -> _ * Result<_ * _, _>) =
    f x s

  member inline __.Using(x, f) =
    use x = x
    f x

  member inline __.TryWith(f, h): Result<'x, 'e> =
    try
      f ()
    with
    | e -> h e

  member inline __.TryFinally(f, g): Result<'x, 'e> =
    try
      f ()
    finally
      g ()

  member inline __.Combine(r, f): Result<'x, 'e> =
    ()

  member inline __.While(guard, f): Result<unit, 'e> =
    let rec loop () =
      if guard () then
        combine (f ()) loop
      else
        Ok ()
    loop ()

  member inline __.For(xs: seq<'x>, f): Result<unit, 'e> =
    use enumerator = xs.GetEnumerator()
    let rec loop () =
      if enumerator.MoveNext() then
        combine (f enumerator.Current) loop
      else
        Ok ()
    loop ()

[<AutoOpen>]
module ResultMutOperators =
  let ( ^> ) (s: 's, x: 'x) (f: 'x -> 's -> 't * 'y) =
    f x s

  let ( *> ) (s: 's, x: 'x) (f: 'x -> 'y) =
    s, f x

  let ( ||^ ) (s, ok) (f: 's -> 's * bool) =
    if ok
    then s, true
    else f s

  let mut = ResultMutBuilder()

module Tests =
  open Xunit

  [<Struct>]
  type Tokenizer =
    {
      Source: string
      Index: int
    }

  module rec Tokenizer =
    let current (self: Tokenizer) =
      self.Index

    let one (self: Tokenizer) =
      if self.Index < self.Source.Length then
        let c = Some (self.Index, self.Source.[self.Index])
        let self = { self with Index = self.Index + 1 }
        self, c
      else
        self, None

    let next (self: Tokenizer) =
      mut {
        let current = self |> current
        match! self |> one with
        | Some (start, ' ') ->
          // return! self |> whitespaceToken current
          return! failwith ""
        | _ -> return! failwith ""
      }

    // let (!) (f: 's -> 's * 'y) =
    //   fun (s, x) ->
    //     let s, y = f s
    //     s, (x, y)

    // let (!!) (f: 't -> 's -> 's * 'y) (t: 't) =
    //   !(f t)

    let (|>!) (s, x) (f: 's -> 't * 'y) =
      let t, y = f s
      t, (x, y)

    let (|>.) (s: 's, (x: 'x, y: 'y)) (f: 'x -> 'y -> 'z): 's * 'z =
      let z = f x y
      s, z

    let eatc expected (self: Tokenizer) =
      match self |> one with
      | self, Some (_, c) when c = expected ->
        self, true
      | _ ->
        self, false

    let whitespaceToken (start: int) self =
      let rec loop self =
        let self, ok = self |> eatc ' ' |>! eatc '\t'
        if ok then loop self else self
      let self = loop self
      self, self.Source.[start..self.Index - 1]

  [<Fact>]
  let stateTest () =
    ()
