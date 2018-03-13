namespace AsterSql.Core

open System
open System.Collections.Generic

type DatabaseType =
  | TInt
  | TString

[<AbstractClass>]
type Column() as this =
  let uniqueName =
    lazy (
      match this.Path.TablePath.ParentPath with
      | Some schemaPath ->
        sprintf "_%s__%s__%s__%s"
          schemaPath.DatabaseName
          schemaPath.SchemaName
          this.Path.TablePath.TableName
          this.Path.ColumnName
      | None ->
        sprintf "%s_%s" this.Path.ColumnName (Guid.NewGuid().ToString().Replace("-", ""))
    )

  abstract Path: ColumnPath
  abstract Type: DatabaseType

  member this.Name =
    this.Path.ColumnName

  member this.UniqueName =
    uniqueName.Value

  member this.CompareTo(other: obj) =
    let other = other :?> Column
    compare this.UniqueName other.UniqueName

  override this.Equals(other: obj) =
    other :? Column && this.CompareTo(other) = 0

  override this.GetHashCode() =
    EqualityComparer<obj>.Default.GetHashCode(this.UniqueName)

  interface IComparable with
    override this.CompareTo(other) =
      this.CompareTo(other)

type Expression =
  | Null
  | Int
    of Long
  | String
    of string
  | Column
    of Column
  | Record
    of list<Column * Expression>
  | ConstantAggregation
    of Expression
  | Add
    of Expression * Expression
  | Max
    of Expression

type Condition =
  | Null
  | True
  | False
  | Not
    of Condition
  | And
    of Condition * list<Condition>
  | IsNull
    of Condition
  | Equal
    of Expression * Expression

type SelectField =
  | Asterisk
    of TablePath
  | Expression
    of Column * Expression

type Order =
  | Ascending
  | Descending

type OrderKey =
  {
    Expression:
      Expression
    Order:
      Order
  }

type SelectStatement =
  {
    Fields:
      list<SelectField>
    From:
      TablePath
  }
with
  static member Create(tablePath) =
    {
      Fields =
        []
      From =
        tablePath
    }
  member this.AddField(expression) =
    { this with
        Fields = Expression expression :: this.Fields
    }

type ValueInsertStatement =
  {
    TablePath:
      TablePath
    Record:
      Map<Column, Expression>
  }
