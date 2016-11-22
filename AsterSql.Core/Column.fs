namespace AsterSql.Core

type Column<'x>(table: Table, columnName: string) =
  let uniqueName = sprintf "__%s__%s" table.Name columnName

  member this.Name = columnName

  member this.Type = DatabaseType.ofType typeof<'x>

  member this.Item
    with get (r: IExpressionRecord) =
      r.[uniqueName]
    and set (r: IExpressionRecord) value =
      r.[uniqueName] <- value

  interface IColumn with
    override this.Name =
      this.Name

    override this.Type =
      this.Type
