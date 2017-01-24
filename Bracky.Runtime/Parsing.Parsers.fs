namespace Bracky.Runtime.Parsing

open System
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

  let keywords =
    [
      "val"
      "true"
      "false"
      "fun"
      "if"
      "else"
    ] |> set

  let keywordParser identifier =
    parse {
      do! skipString identifier
      do! notFollowedBy (letter <|> digit <|> pchar '_')
    }

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
      if keywords |> Set.contains identifier then
        return! fail "Variable name can't be a keyword."
      else
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
      do! notFollowedBy (letter <|> pchar '_')
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
    attempt intExpressionParser
    <|> attempt boolExpressionParser
    <|> parenthesisExpressionParser
    
  let leftAssociatedOperationParser termParser operatorParser ctor =
    chainl1
      termParser
      (attempt (blankParser >>. operatorParser >>. blankParser)
        |>> (fun () left right -> ctor (left, right)))

  let rightAssociatedOperationParser termParser operatorParser ctor =
    chainr1
      termParser
      (attempt (blankParser >>. operatorParser >>. blankParser >>. followedBy termParser)
        |>> (fun () left right -> ctor (left, right)))

  let applyExpressionParser: Parser<Expression> =
    chainl1
      atomicExpressionParser
      (attempt (blankParser >>. followedBy atomicExpressionParser)
        |>> (fun () left right -> ApplyExpression (left, right)))

  let multitiveExpressionParser: Parser<Expression> =
    leftAssociatedOperationParser applyExpressionParser (skipChar '*') MulExpression

  let additiveExpressionParser: Parser<Expression> =
    leftAssociatedOperationParser multitiveExpressionParser (skipChar '+') AddExpression

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
    rightAssociatedOperationParser valExpressionParser (skipChar ';') ThenExpression

  expressionParserRef :=
    thenExpressionParser
