module rec OptLang.Tokenize

open OptLang.Syntax

let private at (text: string) (i: int) =
  if i < text.Length then
    text.[i]
  else
    '\x00'

let private isBlank (c: char) = c = ' ' || c = '\t'

let private isNewline (c: char) = c = '\r' || c = '\n'

let private isDigit (c: char) = '0' <= c && c <= '9'

let private isAlphabet (c: char) =
  ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

let private isIdent (c: char) = isDigit c || isAlphabet c || c = '_'

let private KeywordMap =
  [ "break", Token.Break
    "continue", Token.Continue
    "else", Token.Else
    "false", Token.False
    "fn", Token.Fn
    "if", Token.If
    "loop", Token.Loop
    "then", Token.Then
    "true", Token.True
    "type", Token.Type ]
  |> Map.ofList

let private makeIdentToken (ident: string) =
  match KeywordMap |> Map.tryFind ident with
  | Some it -> it
  | None -> Token.Ident ident

// -----------------------------------------------
// Pos
// -----------------------------------------------

module Pos =
  [<Literal>]
  let Zero: Pos = 0u

  let create y x : Pos = (y <<< 8) ||| x

  let x (p: Pos) = int (p &&& 0xFFu)
  let y (p: Pos) = int (p >>> 8)

  let toPair (p: Pos) = y p, x p
  let ofPair ((y: int), (x: int)) = create (uint y) (uint x)

  // y++, x = 0
  let addNewline (p: Pos) = (p + (1u <<< 8)) ||| (p &&& ~~~ 0xFFu)

  let addSlice (text: string) (l: int) (r: int) (pos: Pos) =
    assert (0 <= l && l <= r && r <= text.Length)

    let rec go pos i =
      if i = r then
        pos
      else if text.[i] = '\n' then
        go (addNewline pos) (i + 1)
      else
        go (pos + 1u) (i + 1)

    go pos l

// -----------------------------------------------
// String
// -----------------------------------------------

let private scanString (text: string) (index: int) =
  let at i = at text i

  let rec scanVerbatim i =
    if i < text.Length then
      match text.[i] with
      | '"'
      | '\\' -> i
      | _ -> scanVerbatim (i + 1)
    else
      i

  let rec go acc i =
    match at i with
    | '"' -> acc, i + 1

    | '\\' ->
      match at (i + 1) with
      | '\\' -> go ("\\" :: acc) (i + 2)
      | '"' -> go ("\"" :: acc) (i + 2)
      | 'n' -> go ("\n" :: acc) (i + 2)
      | c -> failwithf "Unrecognized escape sequence '\\%c'" c

    | '\x00' when i = text.Length -> failwith "Unclosed string literal"

    | _ ->
      let endIndex = scanVerbatim (i + 1)
      go (text.[i .. endIndex - 1] :: acc) endIndex

  assert (index < text.Length && text.[index] = '"')
  let acc, endIndex = go [] (index + 1)
  let value = acc |> List.rev |> String.concat ""
  Token.String value, endIndex - index

// -----------------------------------------------
// Tokenize
// -----------------------------------------------

let private next (text: string) (i: int) =
  let at i = at text i

  let scanDigits i =
    let rec go i =
      if i < text.Length && isDigit (at i) then
        go (i + 1)
      else
        i

    assert (isDigit (at i))
    go (i + 1)

  let scanIdent i =
    let rec go i =
      if isIdent (at i) then go (i + 1) else i

    assert (isAlphabet text.[i] || text.[i] = '_')
    go (i + 1)

  assert (i < text.Length)

  match text.[i] with
  | ' '
  | '\t' ->
    let rec scanBlank i =
      if isBlank (at i) then
        scanBlank (i + 1)
      else
        i

    Token.Blank, scanBlank (i + 1) - i

  | '\r'
  | '\n' ->
    let rec scanNewlines i =
      let c = at i

      if isBlank c || isNewline c then
        scanNewlines (i + 1)
      else
        i

    Token.Newlines, scanNewlines (i + 1) - i

  | '"' -> scanString text i

  | '_' ->
    let endIndex = scanIdent (i + 1)
    makeIdentToken text.[i .. endIndex - 1], endIndex - i

  | '(' -> Token.LeftParen, 1
  | ')' -> Token.RightParen, 1
  | '[' -> Token.LeftBracket, 1
  | ']' -> Token.RightBracket, 1
  | '{' -> Token.LeftBrace, 1
  | '}' -> Token.RightBrace, 1
  | ':' -> Token.Colon, 1
  | ',' -> Token.Comma, 1
  | '.' -> Token.Dot, 1
  | '%' -> Token.Percent, 1
  | '+' -> Token.Plus, 1
  | '*' -> Token.Star, 1

  | '<' ->
    match at (i + 1) with
    | '=' -> Token.LeftEqual, 2
    | _ -> Token.LeftAngle, 1

  | '>' ->
    match at (i + 1) with
    | '=' -> Token.RightEqual, 2
    | _ -> Token.RightAngle, 1

  | '&' ->
    match at 1 with
    | '&' -> Token.AmpAmp, 2
    | _ -> Token.Amp, 1

  | '-' ->
    if at (i + 1) |> isDigit then
      let endIndex = scanDigits (i + 1)
      let value = int text.[i .. endIndex - 1]
      Token.Int value, endIndex - i
    else
      Token.Hyphen, 1

  | '/' ->
    match at (i + 1) with
    | '/' ->
      let rec scanLine i =
        if i < text.Length && not (isNewline text.[i]) then
          scanLine (i + 1)
        else
          i

      Token.Comment, scanLine (i + 2) - i

    | _ -> Token.Slash, 1

  | '|' ->
    match at (i + 1) with
    | '|' -> Token.PipePipe, 2
    | _ -> Token.Pipe, 1

  | '^' -> Token.Hat, 1

  | '=' ->
    match at (i + 1) with
    | '=' -> Token.EqualEqual, 1
    | _ -> Token.Equal, 1

  | c when isDigit c ->
    let endIndex = scanDigits (i + 1)
    let value = int text.[i .. endIndex - 1]
    Token.Int value, endIndex - i

  | c when isIdent c ->
    let endIndex = scanIdent (i + 1)
    makeIdentToken text.[i .. endIndex - 1], endIndex - i

  | _ ->
    let isSafe c =
      isBlank c || isNewline c || c = '(' || c = ')'

    let rec scanBad i =
      if i < text.Length && not (isSafe text.[i]) then
        scanBad (i + 1)
      else
        i

    Token.Bad, scanBad (i + 1) - i

// -----------------------------------------------
// Interface
// -----------------------------------------------

let tokenize (text: string) : (Token * Pos) list =
  let rec go acc (i: int) (pos: Pos) =
    if i < text.Length then
      let token, len = next text i
      assert (len >= 1)

      let r = i + len
      assert (r <= text.Length)

      let acc =
        match token with
        | Token.Blank
        | Token.Newlines
        | Token.Comment -> acc

        | _ -> (token, pos) :: acc

      go acc r (Pos.addSlice text i r pos)
    else
      acc

  go [] 0 Pos.Zero |> List.rev
