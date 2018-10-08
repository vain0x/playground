module TomlHummer.Printing.PrintingTests

open System
open Xunit
open TomlHummer
open TomlHummer.Printing

[<Fact>]
let printTest () =
  let source =
    [
      StmtSyn.Binding ([ExprSyn.Ident "core"],
        ExprSyn.Table [
          [ExprSyn.Ident "autocrlf"], ExprSyn.String "input"
          [ExprSyn.Ident "excludesFile"], ExprSyn.String "~/.gitignore"
        ])
      StmtSyn.Binding ([ExprSyn.Ident "user"; ExprSyn.Ident "name"], ExprSyn.String "John Doe")
      StmtSyn.Binding ([ExprSyn.Ident "user"; ExprSyn.Ident "age"], ExprSyn.Int 18)
    ]
  let actual = print source
  let expected = """core = { autocrlf = "input", excludesFile = "~/.gitignore" }
user.name = "John Doe"
user.age = 18
"""

  actual |> is expected

[<Fact>]
let printDateTests () =
  let source =
    [
      StmtSyn.Binding ([ExprSyn.Ident "date"], ExprSyn.Date (DateTime(2018, 10, 8)))
    ]
  let actual = print source
  let expected = """date = 2018-10-08
"""
  actual |> is expected

[<Fact>]
let printTimeTests () =
  let source =
    [
      StmtSyn.Binding ([ExprSyn.Ident "time"], ExprSyn.Time (TimeSpan (0, 0, 32, 0, 999)))
    ]
  let actual = print source
  let expected = """time = 00:32:00.999
"""
  actual |> is expected
