namespace Bracky.Runtime.Parsing

open FParsec

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Position =
  let empty = Position("", 0L, 0L, 0L)

type Variable =
  {
    Name:
      string
    Id:
      int64
  }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Variable =
  let count = ref 0L

  let create name =
    let identifier = !count
    count := identifier + 1L
    {
      Name = name
      Id = identifier
    }

  let positionFree name =
    {
      Name = name
      Id = -1L
    }

type Pattern =
  | UnitPattern
    of Position
  | VariablePattern
    of Position * Variable
with
  member this.Position =
    match this with
    | UnitPattern position ->
      position
    | VariablePattern (position, _) ->
      position

  member this.PositionFree =
    match this with
    | UnitPattern _ ->
      UnitPattern Position.empty
    | VariablePattern (_, variable) ->
      VariablePattern (Position.empty, Variable.positionFree variable.Name)

type BinaryOperator =
  | ApplyOperator
  | AddOperator
  | MulOperator
  | ThenOperator

type Expression =
  | UnitExpression
    of Position
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
    | UnitExpression position ->
      position
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

  member this.PositionFree =
    match this with
    | UnitExpression _ ->
      UnitExpression Position.empty
    | IntExpression (_, value) ->
      IntExpression (Position.empty, value)
    | BoolExpression (_, value) ->
      BoolExpression (Position.empty, value)
    | RefExpression (_, name) ->
      RefExpression (Position.empty, name)
    | FunExpression (_, pattern, expression) ->
      FunExpression (Position.empty, pattern.PositionFree, expression.PositionFree)
    | IfExpression (head, tail) ->
      let tail = tail |> Array.map (fun c -> c.PositionFree)
      IfExpression (head.PositionFree, tail)
    | BinaryOperationExpression (operator, left, right) ->
      BinaryOperationExpression (operator, left.PositionFree, right.PositionFree)
    | ValExpression (pattern, expression) ->
      ValExpression (pattern.PositionFree, expression.PositionFree)

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

  member this.PositionFree =
    match this with
    | IfClause (condition, expression) ->
      IfClause (condition.PositionFree, expression.PositionFree)
    | ElseClause expression ->
      ElseClause (expression.PositionFree)
