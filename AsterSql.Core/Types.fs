namespace AsterSql.Core

open System

type DbType =
  | TInt
  | TString

type IExpressionRecord =
  abstract Item: string -> IExpression with get, set

type IColumn =
  abstract Name: string
  abstract Type: Type

[<AbstractClass>]
type Table() =
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

  abstract GetTable: string -> Table
