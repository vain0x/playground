namespace AsterSql.Core

module Ast =
  type IExpression =
    abstract member Cast: unit -> Expression<'x>

  and Expression<'x> =
    | Null
    | Int
      of 'x
    | String
      of 'x
    | Add
      of Expression<'x> * Expression<'x>
    | Max
      of Expression<'x>

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
      of IExpression * IExpression

  type SelectField =
    | Asterisk
      of string
    | Expression
      of IExpression * option<string>

  type Order =
    | Ascending
    | Descending

  type OrderKey =
    {
      Expression
        : IExpression
      Order
        : Order
    }

  type SelectStatement =
    {
      Fields
        : SelectField * list<SelectField>
      Where
        : Condition
      GroupBy
        : list<IExpression>
      OrderBy
        : list<OrderKey>
    }
