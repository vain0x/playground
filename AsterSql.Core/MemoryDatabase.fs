namespace AsterSql.Core

open System
open System.Collections.Generic
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

type MemoryTable private (table: Table, records: ResizeArray<_>) =
  inherit MemoryRelation(table, records)

  new(table) =
    MemoryTable(table, ResizeArray())

  member this.Table = table

  member this.Records = records

  member this.Insert(record) =
    records.Add(record)
    0L // TODO: Return generated ID.

type MemoryEntity(database: Database) =
  inherit Entity()

  let schemas =
    database.Schemas |> Map.map
      (fun schemaName schema ->
        schema.Tables |> Map.map
          (fun tableName table ->
            MemoryTable(table)
          )
      )

  let tryFindTable (tablePath: TablePath) =
    schemas
    |> Map.tryFind tablePath.SchemaName
    |> Option.bind (Map.tryFind tablePath.TableName)
  
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
  
  override this.ExecuteSelect(selectStatement) =
    NotImplementedException() |> raise

  override this.ExecuteValueInsert(valueInsertStatement) =
    let table =
      tryFindTable valueInsertStatement.TablePath
      |> Option.get // TODO: throw better exception
    let record =
      table.Table.Columns
      |> Seq.map
        (fun column ->
          match valueInsertStatement.Record |> Map.tryFind column.UniqueName with
          | Some expression ->
            expression |> evaluateExpression
          | None ->
            NotImplementedException() |> raise // TODO: use default value or throw
        )
      |> Seq.toArray
    table.Insert(record)

  override this.Dispose() =
    ()

type MemoryDatabase(databaseName: string) as this =
  inherit Database(DatabasePathRoot.Instance / databaseName)

  let entity =
    lazy (
      new MemoryEntity(this)
    )

  override this.Connect(): Entity =
    entity.Value :> Entity
