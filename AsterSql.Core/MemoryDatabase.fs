namespace AsterSql.Core

open System
open System.Reflection

type DbValue =
  | VNull
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

  let records = ResizeArray()

  let relation =
    lazy (
      MemoryRelation(this, records)
    )

  member this.Insert(record) =
    records.Add(record)
    0L // TODO: Return generated ID.

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

  let rec evaluateExpression (expression: Expression) =
    let eval = evaluateExpression
    match expression with
    | Expression.Null ->
      VNull
    | Int value ->
      match value |> box with
      | null ->
        VNull
      | :? Long as value ->
        value |> VInt
      | _ ->
        NotImplementedException() |> raise
    | String value ->
      match value with
      | null -> VNull
      | _ ->
        value |> string |> VString
    | Add (l, r) ->
      match (l |> eval, r |> eval) with
      | (VNull, _)
      | (_, VNull) ->
        VNull
      | (VInt l, VInt r) ->
        (l + r) |> VInt
      | (VString l, VString r) ->
        (l + r) |> VString
      | _ ->
        NotImplementedException() |> raise
    | Max value ->
      NotImplementedException() |> raise

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
    let table =
      entity.TryGetTable(valueInsertStatement.TableName)
      |> Option.get // TODO: throw better exception
      :?> MemoryTable
    let record =
      table.Columns.Value
      |> Seq.map
        (fun column ->
          match valueInsertStatement.Record |> Map.tryFind column.Name with
          | Some expression ->
            expression |> evaluateExpression
          | None ->
            NotImplementedException() |> raise // TODO: use default value or throw
        )
      |> Seq.toArray
    table.Insert(record)
