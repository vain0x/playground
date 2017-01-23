namespace Bracky.Runtime.Parsing

open System
open FParsec

module Parsers =
  type Parser<'x> =
    Parser<'x, unit>

  let singleLineCommentParser: Parser<unit> =
    skipString "//" >>. skipRestOfLine true

  let blank1Parser: Parser<unit> =
    (attempt singleLineCommentParser <|> spaces1)
    >>. skipSepBy singleLineCommentParser spaces

  let blankParser: Parser<unit> =
    spaces >>. skipSepBy singleLineCommentParser spaces

  let identifierPatternParser: Parser<Pattern> =
    parse {
      let! position = getPosition
      let! headChar = letter <|> pchar '_'
      let! tailChars = manyChars (letter <|> pchar '_' <|> digit)
      let identifier = string headChar + tailChars
      return IdentifierPattern (position, identifier)
    }

  let (expressionParser: Parser<Expression>, expressionParserRef) =
    createParserForwardedToRef ()

  let intExpressionParser: Parser<Expression> =
    parse {
      let! position = getPosition
      let! digits = many1Chars digit
      match Int64.TryParse(digits) with
      | (true, value) ->
        return IntExpression (position, value)
      | (false, _) ->
        return! fail "Invalid integer literal."
    }

  let parenthesisExpressionParser: Parser<Expression> =
    between
      (skipChar '(' >>. blankParser)
      (blankParser >>. skipChar ')')
      expressionParser

  let atomicExpressionParser: Parser<Expression> =
    attempt intExpressionParser
    <|> parenthesisExpressionParser
    
  let leftAssociatedOperationParser termParser operatorParser ctor =
    chainl1
      termParser
      (attempt (blankParser >>. operatorParser >>. blankParser)
        |>> (fun () left right -> ctor (left, right)))

  let rightAssociatedOperationParser termParser operatorParser ctor =
    chainr1
      termParser
      (attempt (blankParser >>. operatorParser >>. blankParser)
        |>> (fun () left right -> ctor (left, right)))

  let multitiveExpressionParser: Parser<Expression> =
    leftAssociatedOperationParser atomicExpressionParser (skipChar '*') MulExpression

  let additiveExpressionParser: Parser<Expression> =
    leftAssociatedOperationParser multitiveExpressionParser (skipChar '+') AddExpression

  let thenExpressionParser: Parser<Expression> =
    rightAssociatedOperationParser additiveExpressionParser (skipChar ';') ThenExpression

  expressionParserRef :=
    thenExpressionParser
