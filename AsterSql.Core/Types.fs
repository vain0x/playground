namespace AsterSql.Core

open System

type DatabaseType =
  | TInt
  | TString

type IExpressionRecord =
  abstract Item: string -> IExpression with get, set

type IColumn =
  abstract Name: string
  abstract Type: DatabaseType

[<AbstractClass>]
type Table() =
  abstract DatabaseName: string
  abstract SchemaName: string
  abstract Name: string

  abstract Columns: ROList<IColumn>

[<AbstractClass>]
type Transaction() =
  abstract Commit: unit -> unit

  abstract Dispose: unit -> unit

  interface IDisposable with
    override this.Dispose() =
      this.Dispose()

[<AbstractClass>]
type DatabaseSchema() =
  abstract Name: string
