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

type IMemoryRecord =
  abstract Item: ColumnPath -> DbValue with get, set

type MemoryRecord =
  array<DbValue>

type MemoryRelation(relation: Relation, records: seq<MemoryRecord>) =
  member this.Relation = relation

  member this.Records = records

  member this.MemoryRecords =
    seq {
      for record in records do
        yield
          { new IMemoryRecord with
              override this.Item
                with get columnPath =
                  match relation.Columns |> Seq.tryFindIndex (fun c -> c.Path = columnPath) with
                  | Some index ->
                    record.[index]
                  | None ->
                    NotImplementedException() |> raise
                and set columnPath (value: DbValue) =
                  match relation.Columns |> Seq.tryFindIndex (fun c -> c.Path = columnPath) with
                  | Some index ->
                    record.[index] <- value
                  | None ->
                    NotImplementedException() |> raise
          }
    }

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
  
  let rec evaluateExpression (source: seq<IMemoryRecord>) (expression: Expression) =
    let eval = evaluateExpression source
    match expression with
    | Expression.Null ->
      VNull |> Seq.replicate (source |> Seq.length)
    | Int value ->
      let value =
        match value |> box with
        | null ->
          VNull
        | :? Long as value ->
          value |> VInt
        | _ ->
          NotImplementedException() |> raise
      value |> Seq.replicate (source |> Seq.length)
    | String value ->
      let value =
        match value with
        | null -> VNull
        | _ ->
          value |> string |> VString
      value |> Seq.replicate (source |> Seq.length)
    | Column columnPath ->
      seq {
        for record in source ->
          record.[columnPath]
      }
    | Add (l, r) ->
      seq {
        for (l, r) in Seq.zip (l |> eval) (r |> eval) ->
          match (l, r) with
          | (VNull, _)
          | (_, VNull) ->
            VNull
          | (VInt l, VInt r) ->
            (l + r) |> VInt
          | (VString l, VString r) ->
            (l + r) |> VString
          | _ ->
            NotImplementedException() |> raise
      }
    | Max value ->
      NotImplementedException() |> raise
  
  override this.ExecuteSelect(selectStatement) =
    let relation =
      tryFindTable selectStatement.From
      |> Option.get
    evaluateExpression relation.MemoryRecords selectStatement.Fields

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
