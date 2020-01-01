module RaiiLang.Helpers

type HashMap<'K, 'V> = System.Collections.Generic.Dictionary<'K, 'V>

type CallBy =
  | ByMove
  | ByRef

let eol = "\n"

let inline cons head tail = head :: tail

let freshName =
  let map = HashMap()

  fun name ->
    match map.TryGetValue(name) with
    | true, lastId ->
      let lastId = lastId + 1
      map.[name] <- lastId
      sprintf "%s_%d" name lastId

    | false, _ ->
      let lastId = 1
      map.[name] <- lastId
      name

// -----------------------------------------------
// Char
// -----------------------------------------------

let charSub (x: char) (y: char) =
  int x - int y

let charIsControl (c: char) =
  let n = int c
  0 <= n && n < 32 || n = 127

let charIsEol (c: char) =
  c = '\r' || c = '\n'

let charIsSpace (c: char) =
  c = ' ' || c = '\t' || c = '　'

let charIsDigit (c: char) =
  '0' <= c && c <= '9'

let charIsAlpha (c: char) =
  ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

let charIsIdentFirst (c: char) =
  c = '_'|| charIsAlpha c

let charIsIdent (c: char) =
  c = '_'|| charIsAlpha c || charIsDigit c

// -----------------------------------------------
// String
// -----------------------------------------------

let strSlice (start: int) (endIndex: int) (s: string): string =
  assert (start <= endIndex && endIndex <= s.Length)
  if start >= endIndex then
    ""
  else
    s.[start..endIndex - 1]
