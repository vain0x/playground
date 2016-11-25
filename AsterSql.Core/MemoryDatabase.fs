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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MemoryRecord =
  let empty =
    { new IMemoryRecord with
        override this.Item
          with get _ =
            InvalidOperationException() |> raise
          and set _ _ =
            InvalidOperationException() |> raise
    }

type MemorySource<'y, 'x> =
  seq<'y * seq<'x>>

type MemorySource =
  MemorySource<list<Column>, IMemoryRecord>

type MemoryTarget =
  seq<DbValue>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MemorySource =
  let single: MemorySource =
    ([], MemoryRecord.empty |> Seq.singleton) |> Seq.singleton

  let ofGroup columns xs: MemorySource =
    (columns, xs) |> Seq.singleton

  let rec convert f _: MemoryTarget =
    todo ()

  let zip _ _: MemorySource<_ * _, _> =
    todo ()

  let aggregate _ _: MemoryTarget =
    todo ()

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

  member this.MemorySource: MemorySource =
    ([], this.MemoryRecords) |> Seq.singleton

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
  
  let rec evaluateExpression (source: MemorySource) (expression: Expression): MemoryTarget =
    let eval = evaluateExpression source
    match expression with
    | Expression.Null ->
      source |> MemorySource.convert (fun _ -> VNull)
    | Int value ->
      let value =
        match value |> box with
        | null ->
          VNull
        | :? Long as value ->
          value |> VInt
        | _ ->
          todo ()
      source |> MemorySource.convert (fun _ -> value)
    | String value ->
      let value =
        match value with
        | null -> VNull
        | _ ->
          value |> string |> VString
      source |> MemorySource.convert (fun _ -> value)
    | Column columnPath ->
      source |> MemorySource.convert (fun record -> record.[columnPath])
    | ConstantAggregation expression ->
      source |> MemorySource.aggregate
        (fun _ xs ->
          expression |> evaluateExpression MemorySource.single
          |> Seq.head
        )
    | Add (l, r) ->
      (l |> eval, r |> eval)
      ||> MemorySource.zip
      |> MemorySource.convert
        (function
          | (VNull, _)
          | (_, VNull) ->
            VNull
          | (VInt l, VInt r) ->
            (l + r) |> VInt
          | (VString l, VString r) ->
            (l + r) |> VString
          | _ ->
            todo ()
        )
    | Max expression ->
      source |> MemorySource.aggregate
        (fun columns xs ->
          expression |> evaluateExpression (MemorySource.ofGroup columns xs)
          |> Seq.max // or VNull
        )

  override this.ExecuteSelect(selectStatement) =
    let relation =
      tryFindTable selectStatement.From
      |> Option.get
    let columns =
      selectStatement.Fields
      |> Seq.collect
        (function
          | Asterisk _ -> todo ()
          | Expression (name, expression) ->
            Seq.singleton (name, expression)
          )
    let record =
      columns |> Seq.map
        (fun (name, expression) ->
          (name, expression |> evaluateExpression relation.MemorySource)
        )
      |> DictionaryValueRecord.ofSeq
    record

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
