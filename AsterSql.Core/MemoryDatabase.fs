namespace AsterSql.Core

open System
open System.Reflection

type DbValue =
  | VInt
    of int64
  | VString 
    of string

type MemoryRecord =
  array<DbValue>

type MemoryRelation(relation: Relation, records: seq<MemoryRecord>) =
  member this.Relation = relation

  member this.Records = records

type MemoryColumn<'x>(columnPath) =
  inherit Column<'x>(columnPath)

[<AbstractClass>]
type MemoryTable(tablePath) as this =
  inherit Table()

  let relation =
    lazy (
      MemoryRelation(this, [||])
    )

  override this.Path = tablePath

type MemoryDatabaseSchema<'entity when 'entity :> Entity>(schemaPath) =
  inherit DatabaseSchema<'entity>()

  let entity: 'entity =
    let arguments = [| schemaPath :> obj |]
    Activator.CreateInstance(typeof<'entity>, arguments) :?> 'entity

  override this.Path = schemaPath

  override this.Connect(): 'entity =
    entity

type MemoryDatabase(databaseName: string) =
  inherit Database()

  override val Path = DatabasePathRoot.Instance / databaseName

  /// Always creates new schema.
  override this.GetSchema<'entity when 'entity :> Entity>(schemaName) =
    MemoryDatabaseSchema<'entity>(this.Path / schemaName) :> DatabaseSchema<'entity>
