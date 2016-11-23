namespace AsterSql.Core

[<AutoOpen>]
module Ast =
  type Expression =
    | Null
    | Int
      of Long
    | String
      of string
    | Column
      of ColumnPath
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
      of string
    | Expression
      of Expression

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
        Map<string, Expression>
    }
