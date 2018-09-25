module TomlHummer.Parsing.ParsingTests

open System
open TomlHummer
open TomlHummer.Parsing
open Xunit

let parseString (source: string) =
  let tokens = Lexing.lex source 0 source.Length
  Parsing.parse tokens

[<Fact>]
let parseTests () =
  let source = """
title = "parse tests"

[point]
x = 1
y = 2

["spec"]
"lang" = "toml"
"""
  let actual = parseString source
  let expected =
    TomlTable [
      "title", TomlValue.String "parse tests"
      "point",
        TomlValue.Table <| TomlTable [
          "x", TomlValue.Int 1
          "y", TomlValue.Int 2
        ]
      "spec",
        TomlValue.Table <| TomlTable [
          "lang", TomlValue.String "toml"
        ]
    ]
  actual |> is expected

[<Fact>]
let parseTestNav () =
  let source = """
[core]
autocrlf = "input"

[core.user]
name = "John Doe"
age = 18

[core.exclude]
file = "~/excludes"
"""
  let actual = parseString source
  let expected =
    TomlTable [
      "core", TomlValue.Table <| TomlTable [
        "autocrlf", TomlValue.String "input"
        "user", TomlValue.Table <| TomlTable [
          "name", TomlValue.String "John Doe"
          "age", TomlValue.Int 18
        ]
        "exclude", TomlValue.Table <| TomlTable [
          "file", TomlValue.String "~/excludes"
        ]
      ]
    ]
  actual |> is expected

[<Fact>]
let parseTestInlineArray () =
  let source = """default = ["debug", "logger"]
"""
  let actual = parseString source
  let expected =
    TomlTable [
      "default", TomlValue.Array [
        TomlValue.String "debug"
        TomlValue.String "logger"
      ]
    ]
  actual |> is expected

[<Fact>]
let parseTestInlineTableL () =
  let source = """
user = { name = "John Doe", age = 18 }
"""
  let actual = parseString source
  let expected =
    TomlTable [
      "user", TomlValue.Table <| TomlTable [
        "name", TomlValue.String "John Doe"
        "age", TomlValue.Int 18
      ]
    ]
  actual |> is expected

[<Fact>]
let parseTestNestedInlineTable () =
  let source = """
core = { user = { name = "John Doe", age = 18 } }
"""
  let actual = parseString source
  let expected =
    TomlTable [
      "core", TomlValue.Table <| TomlTable [
        "user", TomlValue.Table <| TomlTable [
          "name", TomlValue.String "John Doe"
          "age", TomlValue.Int 18
        ]
      ]
    ]
  actual |> is expected

[<Fact>]
let parseTestOpenArrays () =
  let source = """
[[lang]]
id = 1
name = "toml"
[[lang]]
id = 2
name = "json"
"""
  let actual = parseString source
  let expected =
    TomlTable [
      "lang", TomlValue.Array [
        TomlValue.Table <| TomlTable [
          "id", TomlValue.Int 1
          "name", TomlValue.String "toml"
        ]
        TomlValue.Table <| TomlTable [
          "id", TomlValue.Int 2
          "name", TomlValue.String "json"
        ]
      ]
    ]
  actual |> is expected
