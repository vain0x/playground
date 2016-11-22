namespace AsterSql.Core

type Column<'x>(columnPath: ColumnPath) =
  inherit Column()

  let uniqueName =
    sprintf "_%s__%s__%s__%s"
      columnPath.DatabaseName
      columnPath.SchemaName
      columnPath.TableName
      columnPath.ColumnName

  override this.Path =
    columnPath

  override val Type =
    DatabaseType.ofType typeof<'x>

  member this.Item
    with get (r: IExpressionRecord) =
      r.[uniqueName].Cast<'x>()
    and set (r: IExpressionRecord) (value: Expression<'x>) =
      r.[uniqueName] <- value :> IExpression

  member this.Item
    with get (r: IReadOnlyRecord) =
      r.[uniqueName] :?> 'x
