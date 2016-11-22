namespace AsterSql.Core

open System

type DbValue =
  | VInt
    of int64
  | VString 
    of string

type MemoryField =
  {
    Name:
      string
    Type:
      DatabaseType
  }
with
  static member Create(name, typ): MemoryField =
    {
      Name =
        name
      Type =
        typ
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MemoryField =
  let ofColumn (column: IColumn) =
    MemoryField.Create(column.Name, column.Type)

type MemoryRecord =
  array<DbValue>

type MemoryRelation =
  {
    Fields:
      array<MemoryField>
    Records:
      array<MemoryRecord>
  }
with
  static member Empty(fields) =
    {
      Fields =
        fields
      Records =
        [||]
    }

type MemoryColumn<'x>(table: Table, columnName: string) =
  inherit Column<'x>(table, columnName)

type MemoryTable(tableName) =
  inherit Table()

  let columns = ResizeArray<IColumn>()

  let relation =
    lazy (
      let fields = columns |> Seq.map MemoryField.ofColumn |> Seq.toArray
      MemoryRelation.Empty(fields)
    )

  override this.Name = tableName

  override this.Columns = columns :> ROList<_>

type MemoryDatabaseSchema<'entity when 'entity :> Entity>(databaseName, schemaName) =
  inherit DatabaseSchema<'entity>()

  let entity: 'entity =
    let arguments = [| databaseName :> obj; schemaName :> obj |]
    Activator.CreateInstance(typeof<'entity>, arguments) :?> 'entity

  override this.Name = schemaName

  override this.Connect(): 'entity =
    entity

type MemoryDatabase(databaseName: string) =
  inherit Database()

  override this.Name = databaseName

  /// Always creates new schema.
  override this.GetSchema<'entity when 'entity :> Entity>(schemaName) =
    MemoryDatabaseSchema<'entity>(databaseName, schemaName) :> DatabaseSchema<'entity>
