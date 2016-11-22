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
      DbType
  }

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

type MemoryTable(schemaName: string, tableName) =
  inherit Table()

  override this.Name = tableName

  override this.Columns = [||] :> ROList<_>

type MemoryDatabaseSchema<'entity when 'entity :> Entity>(schemaName) =
  inherit DatabaseSchema<'entity>()

  override this.Name = schemaName

  override this.GetTable(tableName) =
    MemoryTable(this.Name, tableName) :> Table

  override this.Connect(): 'entity =
    NotImplementedException() |> raise

type MemoryDatabase(databaseName: string) =
  inherit Database()

  override this.Name = databaseName

  /// Always creates new schema.
  override this.GetSchema<'entity when 'entity :> Entity>(schemaName) =
    MemoryDatabaseSchema<'entity>(schemaName) :> DatabaseSchema<'entity>
