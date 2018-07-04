namespace Bracky.Runtime.Parsing

open System
open DotNetKit.FSharp.ErrorHandling
open FParsec

module private List =
  let decompose =
    function
    | [] -> invalidOp "List has no value."
    | x :: xs -> (x, xs)

module Parsers =
  type private Tree<'x> =
    | Leaf
      of 'x
    | Node
      of Tree<'x> * Tree<'x>

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module private Tree =
    let toArray =
      let rec loop =
        function
        | Leaf x ->
          [|x|]
        | Node (l, r) ->
          Array.append (loop l) (loop r)
      loop

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Array =
    let decompose (this: array<'x>) =
      (this.[0], this.[1..(this.Length - 1)])

  type Parser<'x> =
    Parser<'x, unit>

  /// Identifiers which can't be an identifier pattern.
  let reservedIdentifiers =
    [
      "_"
      "true"
      "false"
      "val"
      "if"
      "else"
    ] |> set

  let singleLineCommentParser: Parser<unit> =
    skipString "//" >>. skipRestOfLine true

  let blank1Parser: Parser<unit> =
    (attempt spaces1 <|> singleLineCommentParser)
    >>. skipSepBy singleLineCommentParser spaces

  let blankParser: Parser<unit> =
    spaces >>. skipSepBy singleLineCommentParser spaces

  let identifierCharParser =
    letter <|> pchar '_' <|> digit

  /// Parser which parses an exact identifier.
  let keywordParser identifier =
    parse {
      do! skipString identifier
      do! notFollowedBy identifierCharParser
    }

  // Parser which parsers a non-reserved identifier.
  let identifierParser: Parser<string> =
    parse {
      do! notFollowedBy digit
      let! identifier = many1Chars identifierCharParser
      if reservedIdentifiers |> Set.contains identifier then
        return! pzero
      else
        return identifier
    }

  let unitParser: Parser<Position> =
    parse {
      let! position = getPosition
      do! between (skipChar '(') (skipChar ')') blankParser
      return position
    }

  let (patternParser: Parser<Pattern>, patternParserRef) =
    createParserForwardedToRef ()

  let variablePatternParser: Parser<Pattern> =
    parse {
      let! position = getPosition
      let! name = identifierParser
      return VariablePattern (position, Variable.create name)
    }

  let unitPatternParser =
    unitParser |>> UnitPattern

  let atomicPatternParser =
    attempt variablePatternParser
    <|> unitPatternParser

  patternParserRef :=
    atomicPatternParser

  let (expressionParser: Parser<Expression>, expressionParserRef) =
    createParserForwardedToRef ()

  let unitExpressionParser =
    unitParser |>> UnitExpression

  let intExpressionParser: Parser<Expression> =
    parse {
      let! position = getPosition
      let! digits = many1Chars digit
      do! notFollowedBy identifierCharParser
      match Int64.TryParse(digits) with
      | (true, value) ->
        return IntExpression (position, value)
      | (false, _) ->
        return! fail "Invalid integer literal."
    }

  let boolExpressionParser: Parser<Expression> =
    parse {
      let! position = getPosition
      let! value =
        attempt (keywordParser "true" >>% true)
        <|> (keywordParser "false" >>% false)
      return BoolExpression (position, value)
    }

  let varExpressionParser: Parser<Expression> =
    parse {
      let! position = getPosition
      let! identifier = identifierParser
      return VarExpression (position, identifier)
    }

  let rightBracketParser: Parser<unit> =
    optional (skipChar ';' >>. blankParser) >>. skipChar '}'

  let valExpressionParser =
    parse {
      let! position = getPosition
      do! skipChar '{' >>. blankParser
      do! keywordParser "val" >>. blankParser
      let! patterns =
        sepBy1 (patternParser .>> blankParser) (notFollowedBy (skipChar '='))
      do! skipChar '=' >>. blankParser
      let! body = expressionParser
      do! blankParser >>. rightBracketParser
      let (pattern, parameters) = patterns |> List.decompose
      let body =
        parameters |> List.rev |> List.fold
          (fun body parameter -> FunExpression (position, parameter, body))
          body
      return ValExpression (position, pattern, body)
    }

  let funExpressionParser: Parser<Expression> =
    parse {
      let! position = getPosition
      do! skipChar '{' >>. blankParser >>. keywordParser "fun" >>. blankParser
      let! pattern = patternParser
      do! blankParser >>. skipString "->" >>. blankParser
      let! expression = expressionParser
      do! blankParser >>. rightBracketParser
      return FunExpression (position, pattern, expression)
    }

  let ifExpressionParser: Parser<Expression> =
    parse {
      let ifClauseParser =
        parse {
          do! keywordParser "if" >>. blankParser
          let! condition = expressionParser
          do! blankParser >>. skipString "->" >>. blankParser
          let! expression = expressionParser
          do! blankParser
          return IfClause (condition, expression)
        }
      let elseClauseParser =
        parse {
          do! keywordParser "else" >>. blankParser
          let! expression = expressionParser
          do! blankParser
          return ElseClause expression
        }
      let clauseParser =
        attempt ifClauseParser <|> elseClauseParser
      let clauseSeparatorParser =
        skipChar ';' >>. blankParser
        >>. followedBy (attempt (keywordParser "if") <|> keywordParser "else")
      do! skipChar '{' >>. blankParser
      let! clauses =
        chainl1
          (clauseParser .>> blankParser |>> Leaf)
          (attempt clauseSeparatorParser |>> (fun () l r -> Node (l, r)))
      do! rightBracketParser
      return
        clauses |> Tree.toArray |> Array.decompose |> IfExpression
    }

  let parenthesisExpressionParser: Parser<Expression> =
    between
      (skipChar '(' >>. blankParser)
      (blankParser >>. optional (skipChar ';' >>. blankParser) >>. skipChar ')')
      expressionParser

  let atomicExpressionParser: Parser<Expression> =
    attempt boolExpressionParser
    <|> attempt varExpressionParser
    <|> attempt intExpressionParser
    <|> attempt valExpressionParser
    <|> attempt funExpressionParser
    <|> attempt ifExpressionParser
    <|> attempt unitExpressionParser
    <|> parenthesisExpressionParser

  let applyExpressionParser: Parser<Expression> =
    parse {
      let! position = getPosition
      let separatorParser =
        parse {
          do! blankParser
          do! followedBy atomicExpressionParser
          return (fun left right -> ApplyExpression (position, left, right))
        }
      return! chainl1 atomicExpressionParser (attempt separatorParser)
    }

  let leftAssociatedOperationParser termParser operatorParser =
    let separatorParser =
      parse {
        do! blankParser
        let! operator = operatorParser
        do! blankParser
        let position = (operator: Expression).Position
        let f left right =
          ApplyExpression (position, ApplyExpression (position, operator, left), right)
        return f
      }
    chainl1 termParser (attempt separatorParser)

  let operatorParser operator representation =
    parse {
      let! position = getPosition
      do! skipString representation
      do! notFollowedBy (skipAnyOf "<>-=+*!?%&|^~:")
      return OperatorExpression(position, operator)
    }

  let multitiveExpressionParser =
    leftAssociatedOperationParser
      applyExpressionParser (operatorParser MulOperator "*")

  let additiveExpressionParser =
    leftAssociatedOperationParser
      multitiveExpressionParser (operatorParser AddOperator "+")

  let thenExpressionParser: Parser<Expression> =
    let termParser = additiveExpressionParser
    let separatorParser =
      parse {
        do! blankParser
        let! position = getPosition
        do! skipChar ';' >>. blankParser
        do! followedBy termParser // TODO: improve
        return (fun left right -> ThenExpression (position, left, right))
      }
    chainr1 termParser (attempt separatorParser)

  expressionParserRef :=
    thenExpressionParser

  let programParser =
    blankParser >>. expressionParser .>> blankParser .>> eof

  let parseExpression sourceName source =
    match runParserOnString programParser () sourceName source with
    | Success (expression, (), _) ->
      Result.Ok expression
    | Failure (message, _, ()) ->
      Result.Error message
