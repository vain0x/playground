namespace rec FadaToml

open System

type Array = Value[]

type Table = (string * Value)[]

[<RequireQualifiedAccess>]
type Value =
  | String of string
  | Int of int64
  | Float of float
  | Bool of bool
  | DateTime of DateTime
  | Array of Array
  | Table of Table

[<RequireQualifiedAccess>]
type Spanned<'T> =
  {
    Start: int
    End: int
    Value: 'T
  }

[<RequireQualifiedAccess>]
type Span =
  {
    Start: int
    End: int
  }

[<RequireQualifiedAccess>]
type Token =
  | Whitespace of string
  | Newline
  | Comment of string
  | Equals
  | Period
  | Comma
  | Colon
  | Plus
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | Keylike of string
  | String of StringToken

[<RequireQualifiedAccess>]
type StringToken =
  {
    Src: string
    Value: string
    Multiline: bool
  }

[<RequireQualifiedAccess>]
type Error =
  | InvalidCharInString
    of int * char
  | InvalidEscape
    of int * char
  | InvalidHexEscape
    of int * char
  | InvalidEscapeValue
    of int * uint32
  | NewlineInString
    of int
  | Unexpected
    of int * char
  | UnterminatedString
    of int
  | NewlineInTableKey
    of int
  | MultilineStringKey
    of int
  | EmptyTableKey
    of int
  | Wanted
    of WantedError

[<RequireQualifiedAccess>]
type WantedError =
  {
    At: int
    Expected: string
    Found: string
  }

[<RequireQualifiedAccess>]
type Tokenizer =
  {
    Input: string
    Chars: CrlfFold
  }

[<RequireQualifiedAccess>]
type CrlfFold =
  {
    Str: string
    Index: int
  }

[<RequireQualifiedAccess>]
type MaybeString =
  | NotEscaped
    of int
  | Owned
    of string

type internal TkResult<'T> = Result<'T, Error>

type internal Mut<'S, 'T> = 'S * 'T
