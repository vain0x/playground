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

  member this.ToMap() =
    dictionary
    |> Seq.map (fun (KeyValue (key, value)) -> (key, value))
    |> Map.ofSeq

  interface IExpressionRecord with
    override this.Item
      with get columnName =
        dictionary.[columnName]
      and set columnName value =
        dictionary.Add(columnName, value)

type Sql() =
  do ()

[<AbstractClass>]
type Entity() =
  abstract ExecuteSelect: SelectStatement -> seq<IReadOnlyRecord>

  abstract ExecuteValueInsert: ValueInsertStatement -> Long

  abstract Dispose: unit -> unit

  interface IDisposable with
    override this.Dispose() =
      this.Dispose()

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
type Table(tablePath) =
  inherit Relation()

  member this.TablePath =
    (tablePath: TablePath)

[<AbstractClass>]
type DatabaseSchema(schemaPath) as this =
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

  member this.Path: DatabaseSchemaPath =
    schemaPath

  member this.DatabaseName =
    this.Path.DatabaseName

  member this.Name =
    this.Path.SchemaName

  member this.Tables =
    tables.Value

[<AbstractClass>]
type Database(databasePath) as this =
  let schemas =
    lazy (
      this.GetType().GetProperties()
      |> Array.choose
        (fun pi ->
          if typeof<DatabaseSchema>.IsAssignableFrom(pi.PropertyType)
            && pi.GetMethod |> isNull |> not
          then
            let schema = pi.GetValue(this) :?> DatabaseSchema
            (schema.Name, schema) |> Some
          else
            None
        )
      |> Map.ofSeq
    )

  abstract Connect: unit -> Entity
  
  member this.Path = databasePath

  member this.Name =
    this.Path.DatabaseName

  member this.Schemas =
    schemas.Value

[<AbstractClass>]
type Transaction() =
  abstract Commit: unit -> unit

  abstract Dispose: unit -> unit

  interface IDisposable with
    override this.Dispose() =
      this.Dispose()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Extension =
  open System.Runtime.CompilerServices

  [<Extension>]
  let Insert (this: Entity, table: Table, assign: Action<IExpressionRecord>) =
    let record = DictionaryExpressionRecord()
    assign.Invoke(record)
    let statement =
      {
        TablePath =
          table.TablePath
        Record =
          record.ToMap()
      }
    this.ExecuteValueInsert(statement)
