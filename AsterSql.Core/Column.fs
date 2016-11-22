namespace AsterSql.Core

type Column<'typ>(table: Table, name: string) =
  let uniqueName = sprintf "__%s__%s" table.Name name

  member this.Name = name

  member this.Type = typeof<'typ>

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
