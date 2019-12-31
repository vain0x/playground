module RaiiLang.SyntaxTokenize

open RaiiLang.Helpers
open RaiiLang.Syntax
open RaiiLang.SyntaxTokenizeContext

type T = TokenizeContext

let charIsPun (c: char) =
  punctuations |> List.exists (fun (_, s) -> s.Contains(c))

let charIsAny (c: char) =
  charIsEol c
  || charIsSpace c
  || charIsIdent c
  || charIsPun c

let charIsOther (c: char) =
  c |> charIsAny |> not

let tokenizeEol (t: T) =
  if t.Eat("\r\n") then
    t.Commit(EolToken)

  if t.Eat("\n") then
    t.Commit(EolToken)

let tokenizeSpace (t: T) =
  if t.Next |> charIsSpace then
    while t.Next |> charIsSpace do
      t.Bump()

    t.Commit(SpaceToken)

let tokenizeComment (t: T) =
  if t.Eat("//") || t.Eat("#") then
    while not t.AtEof && t.Next |> charIsEol |> not do
      t.Bump()

    t.Commit(CommentToken)

let tokenizeInt (t: T) =
  if t.Next |> charIsDigit then
    while t.Next |> charIsDigit do
      t.Bump()

    t.Commit(IntToken)

let tokenizeIdent (t: T) =
  if t.Next |> charIsIdentFirst then
    while t.Next |> charIsIdent do
      t.Bump()

    let ident = t.CurrentText()
    let token =
      keywords |> List.tryPick (fun (token, word) ->
        if word = ident then
          Some token
        else
          None
      )
      |> Option.defaultValue IdentToken
    t.Commit(token)

let tokenizePun (t: T) =
  for token, word in punctuations do
    if t.Eat(word) then
      t.Commit(token)

let tokenizeOther (t: T) =
  if not t.AtEof && t.Next |> charIsOther then
    while not t.AtEof && t.Next |> charIsOther do
      t.Bump()

    t.Commit(OtherToken)

let tokenizeAll (t: T) =
  while not t.AtEof do
    let state = t.CurrentState()

    tokenizeEol t
    tokenizeSpace t
    tokenizeComment t
    tokenizeInt t
    tokenizeIdent t
    tokenizePun t
    tokenizeOther t

    assert (state <> t.CurrentState())
