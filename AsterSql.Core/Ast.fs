namespace AsterSql.Core

[<AutoOpen>]
module Ast =
  type Expression =
    | Null
    | Int
      of Long
    | String
      of string
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
      of Expression * option<string>

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
        SelectField * list<SelectField>
      Where:
        Condition
      GroupBy:
        list<Expression>
    }

  type ValueInsertStatement =
    {
      TablePath:
        TablePath
      Record:
        Map<string, Expression>
    }
