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
  | ApplyExpression
    of Expression * Expression
  | MulExpression
    of Expression * Expression
  | AddExpression
    of Expression * Expression
  | ValExpression
    of Pattern * Expression
  | ThenExpression
    of Expression * Expression
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
    | ApplyExpression (f, _) ->
      f.Position
    | MulExpression (left, _) ->
      left.Position
    | AddExpression (left, _) ->
      left.Position
    | ValExpression (pattern, _) ->
      pattern.Position
    | ThenExpression (left, _) ->
      left.Position

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
    | ApplyExpression (f, x) ->
      ApplyExpression (f.SetPosition(position), x.SetPosition(position))
    | MulExpression (left, right) ->
      MulExpression (left.SetPosition(position), right.SetPosition(position))
    | AddExpression (left, right) ->
      AddExpression (left.SetPosition(position), right.SetPosition(position))
    | ValExpression (pattern, expression) ->
      ValExpression (pattern.SetPosition(position), expression.SetPosition(position))
    | ThenExpression (left, right) ->
      ThenExpression (left.SetPosition(position), right.SetPosition(position))

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
