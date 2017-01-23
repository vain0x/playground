namespace Bracky.Runtime.Parsing

open System
open FParsec

module Parsers =
  type Parser<'x> =
    Parser<'x, unit>

  let singleLineCommentParser: Parser<unit> =
    skipString "//" >>. skipRestOfLine true

  let blank1Parser: Parser<unit> =
    (attempt spaces1 <|> singleLineCommentParser)
    >>. skipSepBy singleLineCommentParser spaces

  let blankParser: Parser<unit> =
    spaces >>. skipSepBy singleLineCommentParser spaces

  let (patternParser: Parser<Pattern>, patternParserRef) =
    createParserForwardedToRef ()

  let identifierPatternParser: Parser<Pattern> =
    parse {
      let! position = getPosition
      do! notFollowedBy digit
      let! identifier = many1Chars (letter <|> pchar '_' <|> digit)
      return IdentifierPattern (position, identifier)
    }

  patternParserRef :=
    identifierPatternParser

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

  let valExpressionParser: Parser<Expression> =
    attempt
      (parse {
        do! skipString "val" >>. blank1Parser
        let! pattern = patternParser
        do! blankParser >>. skipChar '=' >>. blankParser
        let! expression = additiveExpressionParser
        return ValExpression (pattern, expression)
      })
    <|> additiveExpressionParser

  let thenExpressionParser: Parser<Expression> =
    rightAssociatedOperationParser valExpressionParser (skipChar ';') ThenExpression

  expressionParserRef :=
    thenExpressionParser
