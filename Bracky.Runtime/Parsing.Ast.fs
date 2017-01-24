namespace Bracky.Runtime.Parsing

open FParsec

type Pattern =
  | IdentifierPattern
    of Position * string
with
  member this.Position =
    match this with
    | IdentifierPattern (position, _) ->
      position

  member this.SetPosition(position) =
    match this with
    | IdentifierPattern (_, name) ->
      IdentifierPattern (position, name)

type BinaryOperator =
  | ApplyOperator
  | AddOperator
  | MulOperator
  | ThenOperator

type Expression =
  | IntExpression
    of Position * int64
  | BoolExpression
    of Position * bool
  /// Represents an expression to read the value of variable.
  | RefExpression
    of Position * string
  | FunExpression
    of Position * Pattern * Expression
  | IfExpression
    of IfClause * array<IfClause>
  | BinaryOperationExpression
    of BinaryOperator * Expression * Expression
  | ValExpression
    of Pattern * Expression
with
  member this.Position =
    match this with
    | IntExpression (position, _) ->
      position
    | BoolExpression (position, _) ->
      position
    | RefExpression (position, _) ->
      position
    | FunExpression (position, _, _) ->
      position
    | IfExpression (clause, _) ->
      clause.Position
    | BinaryOperationExpression (_, left, _) ->
      left.Position
    | ValExpression (pattern, _) ->
      pattern.Position

  member this.SetPosition(position) =
    match this with
    | IntExpression (_, value) ->
      IntExpression (position, value)
    | BoolExpression (_, value) ->
      BoolExpression (position, value)
    | RefExpression (_, name) ->
      RefExpression (position, name)
    | FunExpression (_, pattern, expression) ->
      let expression = expression.SetPosition(position)
      FunExpression (position, pattern.SetPosition(position), expression)
    | IfExpression (head, tail) ->
      let tail = tail |> Array.map (fun c -> c.SetPosition(position))
      IfExpression (head.SetPosition(position), tail)
    | BinaryOperationExpression (operator, left, right) ->
      let left = left.SetPosition(position)
      let right = right.SetPosition(position)
      BinaryOperationExpression (operator, left, right)
    | ValExpression (pattern, expression) ->
      ValExpression (pattern.SetPosition(position), expression.SetPosition(position))

and IfClause =
  | IfClause
    of Expression * Expression
  | ElseClause
    of Expression
with
  member this.Position =
    match this with
    | IfClause (condition, _) ->
      condition.Position
    | ElseClause expression ->
      expression.Position

  member this.SetPosition(position) =
    match this with
    | IfClause (condition, expression) ->
      IfClause (condition.SetPosition(position), expression.SetPosition(position))
    | ElseClause expression ->
      ElseClause (expression.SetPosition(position))
