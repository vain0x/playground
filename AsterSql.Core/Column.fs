namespace AsterSql.Core

type Column<'x>(columnPath: ColumnPath) =
  inherit Column()

  override this.Path =
    columnPath

  override val Type =
    DatabaseType.ofType typeof<'x>

  member this.Item
    with get (r: IExpressionRecord) =
      r.[this] :?> SqlExpressionBuilder<'x>
    and set (r: IExpressionRecord) (value: SqlExpressionBuilder<'x>) =
      r.[this] <- value :> IExpressionBuilder

  member this.Item
    with get (r: IReadOnlyRecord) =
      r.[this] :?> 'x
