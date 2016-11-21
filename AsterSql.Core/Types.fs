namespace AsterSql.Core

open System

type DbType =
  | TInt
  | TString

type DbValue =
  | VInt
    of int64
  | VString 
    of string

type MemoryField =
  {
    Name
      : string
    Type
      : DbType
  }

type MemoryRecord =
  array<DbValue>

type MemoryRelation =
  {
    Fields
      : array<MemoryField>
    Records
      : array<MemoryRecord>
  }
with
  static member Empty(fields) =
    {
      Fields =
        fields
      Records =
        [||]
    }

type IExpression =
  interface end

type IExpressionRecord =
  abstract member Item: string -> IExpression with get, set

type Column<'typ>(table: Table, name: string) =
  let uniqueName = sprintf "__%s__%s" table.Name name

  member this.Item
    with get (r: IExpressionRecord) =
      r.[uniqueName]
    and set (r: IExpressionRecord) value =
      r.[uniqueName] <- value

and Table(schemaName: string, name: string) =
  member this.SchemaName = schemaName

  member this.Name = name

[<AbstractClass>]
type Transaction() =
  abstract member Commit: unit -> unit

  abstract member Dispose: unit -> unit

  interface IDisposable with
    override this.Dispose() =
      this.Dispose()

[<AbstractClass>]
type Database() =
  abstract member BeginTransaction: unit -> Transaction

[<AbstractClass>]
type Entity() =
  do ()

[<AbstractClass>]
type Database<'entity when 'entity :> Entity>() =
  inherit Database()

  abstract member Connect: unit -> 'entity

type MemoryDatabase<'entity when 'entity :> Entity>() =
  inherit Database<'entity>()

  override this.BeginTransaction() =
    NotImplementedException() |> raise

  override this.Connect(): 'entity =
    NotImplementedException() |> raise
