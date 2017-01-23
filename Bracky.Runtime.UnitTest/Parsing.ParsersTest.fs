namespace Bracky.Runtime.Parsing

open FParsec
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection

module ParsersTest =
  let p =
    Position("", 0L, 0L, 0L)

  let ``test expression parsers`` =
    let body (parser: Parser<Expression, unit>, source, expected: Expression) =
      test {
        match runParserOnString parser () "test" source with
        | Success (actual, (), position) ->
          do! actual.SetPosition(p) |> assertEquals (expected.SetPosition(p))
        | Failure (message, _, _) ->
          return! fail message
      }
    let intParser = Parsers.intExpressionParser
    let parenParser = Parsers.parenthesisExpressionParser
    let mulParser = Parsers.multitiveExpressionParser
    let addParser = Parsers.additiveExpressionParser
    let thenParser = Parsers.thenExpressionParser
    let i value = IntExpression (p, value)
    let add left right = AddExpression (left, right)
    let mul left right = MulExpression (left, right)
    let then' left right = ThenExpression (left, right)
    parameterize {
      case (intParser, "1", i 1L)
      case (intParser, "12", i 12L)
      case (intParser, "9876543210", i 9876543210L)
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
      case (thenParser, "1 ; 2 ; 3", then' (i 1L) (then' (i 2L) (i 3L)))
      run body
    }
