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
