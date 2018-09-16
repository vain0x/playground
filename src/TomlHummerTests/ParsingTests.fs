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
