namespace AsterSql.Core

type Column<'x>(columnPath: ColumnPath) =
  inherit Column()

  let uniqueName =
    sprintf "__%s__%s" columnPath.TableName columnPath.ColumnName

  override this.Path =
    columnPath

  override val Type =
    DatabaseType.ofType typeof<'x>

  member this.Item
    with get (r: IExpressionRecord) =
      r.[uniqueName]
    and set (r: IExpressionRecord) value =
      r.[uniqueName] <- value
