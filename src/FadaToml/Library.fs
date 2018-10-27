namespace rec FadaToml

module Error =
  let wanted (at: int) (expected: string) (found: string) =
    Error.Wanted {
      At = at
      Expected = expected
      Found = found
    }

module Token =
  type Z = Token

  let describe (token: Z) =
    failwith "noop"

module Tokenizer =
  type Z = Tokenizer

  let mk (input: string): Z =
    let self =
      ({
        Input = input
        Chars =
          {
            Str = input
            Index = 0
          }
      }: Tokenizer)
    // FIXME: Eat BOM
    // self |> eatc "\ufeff"
    self

  let next (self: Z): Z * TkResult<option<Span * Token>> = (self, failwith "")

  let peek (self: Z) =
    self |> next |> snd

  let eat (expected: Token) (self: Z) =
    let self, r = self |> eatSpanned expected
    self, r |> Result.map Option.isSome

  let eatSpanned (expected: Token) (self: Z) =
    match self |> next with
    | _, Error e ->
      self, Error e
    | self, Ok (Some (span, found)) when expected = found ->
      self, Ok (Some span)
    | _, Ok _ ->
      self, Ok None

  let expect self =
    match self |> expectSpanned with
    | _, Error e -> self, Error e
    | self, Ok _ -> self, Ok ()

  let expectSpanned expected self: TkResult<Span> =
    let current = self |> current
    match self |> next with
    | _, Error -> self, Error e
    | self, Ok (Some (span, found)) when found = expected ->
      self, Ok span
    | self, Ok (Some (span, found)) ->
      let e =
        Error.Wanted {
          At = self.Input.Length
          Expected = expected |> Token.describe
          Found = "eof"
        }
      self, Error e

  let tableKey (self: Z): Z * TkResult<Span * string> =
    let current = self |> current
    match self |> next with
    | _, Error e -> self, Error e
    | self, Ok (Error)

;