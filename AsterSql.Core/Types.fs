namespace AsterSql.Core

open System
open System.Collections.Generic
open System.Reflection

type DatabaseType =
  | TInt
  | TString

type IReadOnlyRecord =
  abstract Item: string -> obj with get, set

type IExpressionRecord =
  abstract Item: string -> Expression with get, set

type DictionaryExpressionRecord() =
  let dictionary = dict []

  interface IExpressionRecord with
    override this.Item
      with get columnName =
        dictionary.[columnName]
      and set columnName value =
        dictionary.Add(columnName, value)

type Sql() =
  do ()

[<AbstractClass>]
type Column() =
  abstract Path: ColumnPath
  abstract Type: DatabaseType

  member this.Name =
    this.Path.ColumnName

[<AbstractClass>]
type Relation() as this =
  let columns =
    lazy (
      this.GetType().GetProperties()
      |> Array.choose
        (fun pi ->
          if
            typeof<Column>.IsAssignableFrom(pi.PropertyType)
            && pi.GetMethod |> isNull |> not
          then pi.GetValue(this) :?> Column |> Some
          else None
        )
      :> ROList<_>
    )

  member this.Columns = columns

[<AbstractClass>]
type Table() =
  inherit Relation()

  abstract TablePath: TablePath

[<AbstractClass>]
type DatabaseSchema() =
  abstract Path: DatabaseSchemaPath

  member this.DatabaseName =
    this.Path.DatabaseName

  member this.Name =
    this.Path.SchemaName

[<AbstractClass>]
type Entity() as this =
  let tables =
    lazy (
      this.GetType().GetProperties()
      |> Array.choose
        (fun pi ->
          if typeof<Table>.IsAssignableFrom(pi.PropertyType)
            && pi.GetMethod |> isNull |> not
          then
            let table = pi.GetValue(this) :?> Table
            (table.TablePath.TableName, table) |> Some
          else
            None
        )
      |> Map.ofSeq
    )

  member this.TryGetTable(tableName) =
    tables.Value |> Map.tryFind tableName

  member val Sql = Sql()

  abstract Dispose: unit -> unit

  interface IDisposable with
    override this.Dispose() =
      this.Dispose()

[<AbstractClass>]
type DatabaseSchema<'entity when 'entity :> Entity>() =
  inherit DatabaseSchema()

  abstract Connect: unit -> 'entity
  
[<AbstractClass>]
type Database() =
  abstract Path: DatabasePath

  abstract GetSchema<'entity when 'entity :> Entity> :
    string -> DatabaseSchema<'entity>

  abstract ExecuteSelect<'entity when 'entity :> Entity> :
    'entity * SelectStatement -> seq<IReadOnlyRecord>

  abstract ExecuteValueInsert<'entity when 'entity :> Entity> :
    'entity * ValueInsertStatement -> Long

  member this.Name =
    this.Path.DatabaseName

[<AbstractClass>]
type Transaction() =
  abstract Commit: unit -> unit

  abstract Dispose: unit -> unit

  interface IDisposable with
    override this.Dispose() =
      this.Dispose()
