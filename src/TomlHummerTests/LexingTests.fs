module TomlHummer.Lexing.LexingTests

open System
open Xunit
open TomlHummer
open TomlHummer.Lexing

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
