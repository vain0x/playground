namespace rec TomlHummer

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

  type TomlTable =
    | TomlTable
      of list<string * TomlValue>

  [<RequireQualifiedAccess>]
  type TomlValue =
    | Int
      of int
    | String
      of string
    | Array
      of TomlValue list
    | Table
      of TomlTable

  [<RequireQualifiedAccess>]
  type ExprSyn =
    | Int
      of int
    | Ident
      of string
    | String
      of string
    | Array
      of ExprSyn list
    | Table
      of (ExprSyn list * ExprSyn) list

  [<RequireQualifiedAccess>]
  type StmtSyn =
    | Binding
      of ExprSyn list * ExprSyn
    | Table
      of ExprSyn list
    | Array
      of ExprSyn list
