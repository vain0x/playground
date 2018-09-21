module TomlHummer.Printing.PrintingTests

open System
open Xunit
open TomlHummer
open TomlHummer.Printing

[<Fact>]
let printTest () =
  let source =
    [
      StmtSyn.Binding ([ExprSyn.Ident "user"; ExprSyn.Ident "name"], ExprSyn.String "John Doe")
      StmtSyn.Binding ([ExprSyn.Ident "user"; ExprSyn.Ident "age"], ExprSyn.Int 18)
    ]
  let actual = print source
  let expected = """user.name = "John Doe"
user.age = 18
"""

  actual |> is expected
