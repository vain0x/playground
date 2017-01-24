namespace Bracky.Runtime.Parsing

open FParsec
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection

module ParsersTest =
  let p =
    Position("", 0L, 0L, 0L)

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
    let intParser = Parsers.intExpressionParser
    let boolParser = Parsers.boolExpressionParser
    let refParser = Parsers.refExpressionParser
    let parenParser = Parsers.parenthesisExpressionParser
    let funParser = Parsers.funExpressionParser
    let ifParser = Parsers.ifExpressionParser
    let mulParser = Parsers.multitiveExpressionParser
    let addParser = Parsers.additiveExpressionParser
    let valParser = Parsers.valExpressionParser
    let thenParser = Parsers.thenExpressionParser

    let id' name = IdentifierPattern (p, name)

    let i value = IntExpression (p, value)
    let true' = BoolExpression (p, true)
    let false' = BoolExpression (p, false)
    let ref' identifier = RefExpression (p, identifier)
    let fun' pattern body = FunExpression (p, pattern, body)
    let if' hc hx tail = IfExpression (IfClause (hc, hx), tail)
    let add left right = BinaryOperationExpression (AddOperator, left, right)
    let mul left right = BinaryOperationExpression (MulOperator, left, right)
    let val' pattern expression = ValExpression (pattern, expression)
    let then' left right = BinaryOperationExpression (ThenOperator, left, right)

    let ``test expression parsers`` =
      let body (parser: Parser<Expression, unit>, source, expected: Expression) =
        test {
          let! actual = assertParse parser source
          let actual = actual |> Option.map (fun x -> x.SetPosition(p))
          let expected = expected.SetPosition(p) |> Some
          do! actual |> assertEquals expected
        }
      parameterize {
        case (intParser, "1", i 1L)
        case (intParser, "12", i 12L)
        case (intParser, "9876543210", i 9876543210L)
        case (boolParser, "true", true')
        case (boolParser, "false", false')
        case (refParser, "x", ref' "x")
        case (parenParser, "(12)", i 12L)
        case (parenParser, "( 12 )", i 12L)
        case (mulParser, "2*3", mul (i 2L) (i 3L))
        case (mulParser, "2 * 3 * 4", mul (mul (i 2L) (i 3L)) (i 4L))
        case (addParser, "1+2", add (i 1L) (i 2L))
        case (addParser, "1 + 2", add (i 1L) (i 2L))
        case (addParser, "1 + 2 + 3", add (add (i 1L) (i 2L)) (i 3L))
        case (addParser, "1 + (2 + 3)", add (i 1L) (add (i 2L) (i 3L)))
        case (addParser, "2 * 3 + 4", add (mul (i 2L) (i 3L)) (i 4L))
        case (addParser, "2 + 3 * 4", add (i 2L) (mul (i 3L) (i 4L)))
        case (valParser, "val x = 1", val' (id' "x") (i 1L))
        case
          ( thenParser
          , "val x = 1 + 2; val y = 3"
          , then' (val' (id' "x") (add (i 1L) (i 2L))) (val' (id' "y") (i 3L))
          )
        case
          ( thenParser
          , "1 ; 2 ; 3"
          , then' (i 1L) (then' (i 2L) (i 3L))
          )
        case (funParser, "{fun x -> 0}", fun' (id' "x") (i 0L))
        case
          ( funParser
          , "{ fun x -> val y = 1; 2; }"
          , fun' (id' "x") (then' (val' (id' "y") (i 1L)) (i 2L))
          )
        case
          ( ifParser
          , "{if true -> 1}"
          , if' true' (i 1L) [||]
          )
        case
          ( ifParser
          , "{if true -> 1;}"
          , if' true' (i 1L) [||]
          )
        case
          ( ifParser
          , "{ if true -> 1; else 2 }"
          , if' true' (i 1L) [|ElseClause (i 2L)|]
          )
        case
          ( ifParser
          , "{ if true -> val x = 1; 2; if false -> 3; else 4 }"
          , if' true' (then' (val' (id' "x") (i 1L)) (i 2L))
              [|IfClause (false', (i 3L)); ElseClause (i 4L)|]
          )
        case
          ( ifParser
          , "{ if true -> val x = 1; 2; else 3; }"
          , if' true' (then' (val' (id' "x") (i 1L)) (i 2L)) [|ElseClause (i 3L)|]
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
        case (refParser, "_")
        case (refParser, "true")
        case (intParser, "1x")
        case (thenParser, "1;;2")
        run body
      }
