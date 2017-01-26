namespace Bracky.Runtime.Parsing

open FParsec
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection

module ExpressionBuilders =
  let p = Position.empty

  let pVar name = VariablePattern (p, Variable.positionFree name)

  let vUnit = UnitExpression p
  let vInt value = IntExpression (p, value)
  let vTrue = BoolExpression (p, true)
  let vFalse = BoolExpression (p, false)
  let vOp operator = OperatorExpression (p, operator)
  let vVar identifier = VarExpression (p, identifier)
  let vFun pattern body = FunExpression (p, pattern, body)
  let vIf hc hx tail = IfExpression (IfClause (hc, hx), tail)
  let vApp left right = ApplyExpression (p, left, right)
  let vAdd left right = vApp (vApp (vOp AddOperator) left) right
  let vMul left right = vApp (vApp (vOp MulOperator) left) right
  let vVal pattern expression = ValExpression (pattern, expression)
  let vThen left right = ThenExpression (p, left, right)

module ParsersTest =
  let assertParse parser source =
    test {
      match runParserOnString (parser .>> eof) () "test" source with
      | Success (actual, _, _) ->
        return actual |> Some
      | Failure (message, _, _) ->
        do! fail message
        return None
    }

  let ``test blank1Parser`` =
    let body source =
      test {
        let! result = assertParse Parsers.blank1Parser source
        do! result.IsSome |> assertPred
      }
    parameterize {
      case (" ")
      case ("// x")
      run body
    }

  module ExpressionParserTest =
    open ExpressionBuilders

    let intParser = Parsers.intExpressionParser
    let boolParser = Parsers.boolExpressionParser
    let varParser = Parsers.varExpressionParser
    let parenParser = Parsers.parenthesisExpressionParser
    let funParser = Parsers.funExpressionParser
    let ifParser = Parsers.ifExpressionParser
    let mulParser = Parsers.multitiveExpressionParser
    let addParser = Parsers.additiveExpressionParser
    let valParser = Parsers.valExpressionParser
    let thenParser = Parsers.thenExpressionParser

    let ``test expression parsers`` =
      let body (parser: Parser<Expression, unit>, source, expected: Expression) =
        test {
          let! actual = assertParse parser source
          let actual = actual |> Option.map (fun x -> x.PositionFree)
          let expected = expected.PositionFree |> Some
          do! actual |> assertEquals expected
        }
      parameterize {
        case (intParser, "1", vInt 1L)
        case (intParser, "12", vInt 12L)
        case (intParser, "9876543210", vInt 9876543210L)
        case (boolParser, "true", vTrue)
        case (boolParser, "false", vFalse)
        case (varParser, "x", vVar "x")
        case (parenParser, "(12)", vInt 12L)
        case (parenParser, "( 12 )", vInt 12L)
        case (mulParser, "2*3", vMul (vInt 2L) (vInt 3L))
        case (mulParser, "2 * 3 * 4", vMul (vMul (vInt 2L) (vInt 3L)) (vInt 4L))
        case (addParser, "1+2", vAdd (vInt 1L) (vInt 2L))
        case (addParser, "1 + 2", vAdd (vInt 1L) (vInt 2L))
        case (addParser, "1 + 2 + 3", vAdd (vAdd (vInt 1L) (vInt 2L)) (vInt 3L))
        case (addParser, "1 + (2 + 3)", vAdd (vInt 1L) (vAdd (vInt 2L) (vInt 3L)))
        case (addParser, "2 * 3 + 4", vAdd (vMul (vInt 2L) (vInt 3L)) (vInt 4L))
        case (addParser, "2 + 3 * 4", vAdd (vInt 2L) (vMul (vInt 3L) (vInt 4L)))
        case (valParser, "val x = 1", vVal (pVar "x") (vInt 1L))
        case
          ( thenParser
          , "val x = 1 + 2; val y = 3"
          , vThen (vVal (pVar "x") (vAdd (vInt 1L) (vInt 2L))) (vVal (pVar "y") (vInt 3L))
          )
        case
          ( thenParser
          , "1 ; 2 ; 3"
          , vThen (vInt 1L) (vThen (vInt 2L) (vInt 3L))
          )
        case (funParser, "{fun x -> 0}", vFun (pVar "x") (vInt 0L))
        case
          ( funParser
          , "{ fun x -> val y = 1; 2; }"
          , vFun (pVar "x") (vThen (vVal (pVar "y") (vInt 1L)) (vInt 2L))
          )
        case
          ( ifParser
          , "{if true -> 1}"
          , vIf vTrue (vInt 1L) [||]
          )
        case
          ( ifParser
          , "{if true -> 1;}"
          , vIf vTrue (vInt 1L) [||]
          )
        case
          ( ifParser
          , "{ if true -> 1; else 2 }"
          , vIf vTrue (vInt 1L) [|ElseClause (vInt 2L)|]
          )
        case
          ( ifParser
          , "{ if true -> val x = 1; 2; if false -> 3; else 4 }"
          , vIf vTrue (vThen (vVal (pVar "x") (vInt 1L)) (vInt 2L))
              [|IfClause (vFalse, (vInt 3L)); ElseClause (vInt 4L)|]
          )
        case
          ( ifParser
          , "{ if true -> val x = 1; 2; else 3; }"
          , vIf vTrue (vThen (vVal (pVar "x") (vInt 1L)) (vInt 2L)) [|ElseClause (vInt 3L)|]
          )
        run body
      }

    let ``test expression parser failure`` =
      let body (parser, source) =
        test {
          match runParserOnString (parser .>> eof) () "test" source with
          | Success (value, _, _) ->
            do! fail (sprintf "Success with %s" (string value))
          | Failure _ ->
            return ()
        }
      parameterize {
        case (varParser, "_")
        case (varParser, "true")
        case (intParser, "1x")
        case (thenParser, "1;;2")
        run body
      }
