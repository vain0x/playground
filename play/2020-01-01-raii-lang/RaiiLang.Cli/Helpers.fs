module RaiiLang.Helpers

type HashSet<'K> = System.Collections.Generic.HashSet<'K>

type HashMap<'K, 'V> = System.Collections.Generic.Dictionary<'K, 'V>

[<Struct>]
type StrSegment =
  | StrVerbatim
    of string

/// 間接性
type Indirection =
  /// ポインタを介さない。
  | Direct

  /// ポインタ (参照) を介す。
  | Indirect

/// 可変性
type Mutability =
  /// 読み取り専用
  | ReadOnly

  /// 読み書き可能
  | ReadWrite

type Mode =
  /// indirect, read-only
  | InMode

  /// direct, read-write
  | MutMode

  /// indirect, read-write
  | RefMode

  /// direct, read-only
  | ValMode

type PassBy =
  /// indirect, read-only
  | ByIn

  /// direct
  | ByMove

  /// indirect, read-write
  | ByRef

let eol = "\n"

let inline is< ^T when ^T : equality> (actual: ^T) (expected: ^T) =
  assert (actual = expected)

let inline cons head tail = head :: tail

let freshNameFun () =
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

// -----------------------------------------------
// Mode
// -----------------------------------------------

let modeToString mode =
  match mode with
  | InMode ->
    "in"

  | MutMode ->
    "mut"

  | RefMode ->
    "ref"

  | ValMode ->
    "val"

let modeToPassBy mode =
  match mode with
  | InMode ->
    ByIn

  | MutMode ->
    ByMove

  | RefMode ->
    ByRef

  | ValMode ->
    ByMove

// -----------------------------------------------
// PassBy
// -----------------------------------------------

let passByToString passBy =
  match passBy with
  | ByIn ->
    "in"

  | ByMove ->
    "move"

  | ByRef ->
    "ref"

let passByToMode passBy =
  match passBy with
  | ByIn ->
    InMode

  | ByMove ->
    MutMode

  | ByRef ->
    RefMode
