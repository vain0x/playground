namespace AsterSql.Core

type IExpressionBuilder =
  abstract ToAst: unit -> Expression

type SqlExpressionBuilder<'x>(toAst: unit -> Expression) =
  let create toAst = SqlExpressionBuilder<'y>(toAst)

  member this.Add(r: SqlExpressionBuilder<'x>) =
    (fun () -> Add (this.ToAst(), r.ToAst())) |> create

  member this.Max() =
    (fun () -> Max (this.ToAst())) |> create

  member this.ToAst() =
    toAst ()

  interface IExpressionBuilder with
    override this.ToAst() =
      this.ToAst()
  
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SqlExpression =
  let private create toAst = SqlExpressionBuilder<'y>(toAst)

  let Null () =
    (fun () -> Expression.Null) |> create

  let Int value =
    (fun () -> Int value) |> create

  let String value =
    (fun () -> String value) |> create
