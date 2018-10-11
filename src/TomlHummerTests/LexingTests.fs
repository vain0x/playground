module TomlHummer.Lexing.LexingTests

open System
open Xunit
open TomlHummer
open TomlHummer.Lexing

let lexStr (source: string) =
  Lexing.lex source 0 source.Length

[<Fact>]
let lexTests () =
  let source = """
title = "lex test"

[[points]]
p = 1
"""
  let actual = Lexing.lex source 0 source.Length
  let expected =
    [
      TomlToken.Ident "title"
      TomlToken.Eq
      TomlToken.String "lex test"
      TomlToken.BracketLL
      TomlToken.Ident "points"
      TomlToken.BracketRR
      TomlToken.Ident "p"
      TomlToken.Eq
      TomlToken.Int 1
      TomlToken.Eof
    ]

  actual |> is expected

[<Fact>]
let lexMultilineStr () =
  let source = "str = \"\"\"
Roses are red
Violets are blue\"\"\""
  let expected =
    [
      TomlToken.Ident "str"
      TomlToken.Eq
      TomlToken.String "Roses are red\nViolets are blue"
      TomlToken.Eof
    ]
  source |> lexStr |> is expected

[<Fact>]
let lexLocalDate () =
  let source = """date = 2018-10-08"""
  let expected =
    [
      TomlToken.Ident "date"
      TomlToken.Eq
      TomlToken.Date (DateTime (2018, 10, 8))
      TomlToken.Eof
    ]
  Lexing.lex source 0 source.Length |> is expected

[<Fact>]
let lexLocalTime () =
  let source = """
time = 00:32:00.999999
"""
  let actual = Lexing.lex source 0 source.Length
  let expected =
    [
      TomlToken.Ident "time"
      TomlToken.Eq
      TomlToken.Time (TimeSpan (0, 0, 32, 0, 999))
      TomlToken.Eof
    ]

  actual |> is expected
