namespace KlacFs

open Parser
open Inspector

type KlacFs() = 
    member this.TryParse (source: string) =
        parse_klac_source source

    member this.Inspect (expr: AST.Expr) =
        expr.Inspect()

