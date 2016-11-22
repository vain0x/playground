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

  override this.TablePath = tablePath

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

  let schemas = dict []

  member this.GetOrCreateSchema<'entity when 'entity :> Entity>(schemaName) =
    match schemas.TryGetValue(schemaName) with
    | (true, schema) ->
      schema
    | (false, _) ->
      let schema = MemoryDatabaseSchema<'entity>(this.Path / schemaName) :> DatabaseSchema
      schemas.Add(schemaName, schema)
      schema

  override val Path = DatabasePathRoot.Instance / databaseName

  override this.GetSchema<'entity when 'entity :> Entity>(schemaName) =
     this.GetOrCreateSchema(schemaName) :?> DatabaseSchema<'entity>

  override this.ExecuteSelect<'entity when 'entity :> Entity>
    ( entity: 'entity
    , selectStatement
    ) =
    NotImplementedException() |> raise

  override this.ExecuteValueInsert<'entity when 'entity :> Entity>
    ( entity: 'entity
    , valueInsertStatement
    ) =
    NotImplementedException() |> raise
