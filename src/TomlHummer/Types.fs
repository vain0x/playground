namespace rec TomlHummer

  open System

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
    | Time
      of TimeSpan
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
    | Bool
      of bool
    | Int
      of int
    | String
      of string
    | Time
      of TimeSpan
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
    | Time
      of TimeSpan
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

  [<AutoOpen>]
  module Operators =
    [<Literal>]
    let Day = TimeSpan.TicksPerDay

    let internal timeOfDay (t: TimeSpan) =
      TimeSpan.FromTicks((t.Ticks % Day + Day) % Day)
