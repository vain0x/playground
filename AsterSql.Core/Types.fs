﻿namespace AsterSql.Core

open System
open System.Reflection

type DatabaseType =
  | TInt
  | TString

type IReadOnlyRecord =
  abstract Item: string -> obj with get, set

type ExpressionRecord =
  abstract Item: string -> Expression with get, set

[<AbstractClass>]
type Column() =
  abstract Path: ColumnPath
  abstract Type: DatabaseType

  member this.Name =
    this.Path.ColumnName

[<AbstractClass>]
type Relation() as this =
  let columns =
    lazy (
      this.GetType().GetProperties()
      |> Array.choose
        (fun pi ->
          if
            typeof<Column>.IsAssignableFrom(pi.PropertyType)
            && pi.GetMethod |> isNull |> not
          then pi.GetValue(this) :?> Column |> Some
          else None
        )
      :> ROList<_>
    )

  member this.Columns = columns

[<AbstractClass>]
type Table() =
  inherit Relation()

  abstract TablePath: TablePath

[<AbstractClass>]
type DatabaseSchema() =
  abstract Path: DatabaseSchemaPath

  member this.DatabaseName =
    this.Path.DatabaseName

  member this.Name =
    this.Path.SchemaName

[<AbstractClass>]
type Transaction() =
  abstract Commit: unit -> unit

  abstract Dispose: unit -> unit

  interface IDisposable with
    override this.Dispose() =
      this.Dispose()
