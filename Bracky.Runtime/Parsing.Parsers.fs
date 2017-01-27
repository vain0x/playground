namespace Bracky.Runtime.Parsing

open System
open DotNetKit.FSharp.ErrorHandling
open FParsec

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

  let (patternParser: Parser<Pattern>, patternParserRef) =
    createParserForwardedToRef ()

  let variablePatternParser: Parser<Pattern> =
    parse {
      let! position = getPosition
      let! name = identifierParser
      return VariablePattern (position, VariableOccurrence.create name)
    }

  patternParserRef :=
    variablePatternParser

  let (expressionParser: Parser<Expression>, expressionParserRef) =
    createParserForwardedToRef ()

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

  let refExpressionParser: Parser<Expression> =
    parse {
      let! position = getPosition
      let! identifier = identifierParser
      return RefExpression (position, identifier)
    }

  let rightBracketParser: Parser<unit> =
    optional (skipChar ';' >>. blankParser) >>. skipChar '}'

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
    <|> attempt refExpressionParser
    <|> attempt intExpressionParser
    <|> attempt funExpressionParser
    <|> attempt ifExpressionParser
    <|> parenthesisExpressionParser
    
  let leftAssociatedOperationParser termParser operatorParser operator =
    chainl1
      termParser
      (attempt (blankParser >>. operatorParser >>. blankParser)
        |>> (fun () left right -> BinaryOperationExpression (operator, left, right)))

  let rightAssociatedOperationParser termParser operatorParser operator =
    chainr1
      termParser
      (attempt (blankParser >>. operatorParser >>. blankParser >>. followedBy termParser)
        |>> (fun () left right -> BinaryOperationExpression (operator, left, right)))

  let applyExpressionParser: Parser<Expression> =
    chainl1
      atomicExpressionParser
      (attempt (blankParser >>. followedBy atomicExpressionParser)
        |>> (fun () left right -> BinaryOperationExpression (ApplyOperator, left, right)))

  let multitiveExpressionParser: Parser<Expression> =
    leftAssociatedOperationParser applyExpressionParser (skipChar '*') MulOperator

  let additiveExpressionParser: Parser<Expression> =
    leftAssociatedOperationParser multitiveExpressionParser (skipChar '+') AddOperator

  let valExpressionParser: Parser<Expression> =
    attempt
      (parse {
        do! keywordParser "val" >>. blankParser
        let! pattern = patternParser
        do! blankParser >>. skipChar '=' >>. blankParser
        let! expression = additiveExpressionParser
        return ValExpression (pattern, expression)
      })
    <|> additiveExpressionParser

  let thenExpressionParser: Parser<Expression> =
    rightAssociatedOperationParser valExpressionParser (skipChar ';') ThenOperator

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
