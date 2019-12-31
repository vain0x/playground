module RaiiLang.Helpers

let inline cons head tail = head :: tail

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
