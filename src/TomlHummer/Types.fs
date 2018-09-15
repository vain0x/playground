namespace TomlHummer

  type Ident = string

  [<RequireQualifiedAccess>]
  type TomlToken =
    | Eof
    | Ident
      of string
    | Int
      of int
    | String
      of string
    | Eq
    | Comma
    | Dot
    | BraceL
    | BraceR
    | BracketL
    | BracketR
    | BracketLL
    | BracketRR

  [<RequireQualifiedAccess>]
  type TomlValue =
    | Int
      of int
    | String
      of string
    | Table
      of TomlValue
