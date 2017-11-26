namespace VainZero.Temali.Syntax

open FParsec
open System.Reflection

type Expression =
  | Atom
    of string
  | If
    of Expression

module Parsing =
  type Parser<'TResult> = Parser<'TResult, unit>

  let documentParser: Parser<unit> =
    parse {
      let! _ = pchar 'a'
      return ()
    }

  let inputParser =
    parse {
      do! spaces
      let! statement = documentParser
      do! spaces >>. eof
      return statement
    }

  let tryParse source parser =
    match runParserOnString parser () "input" source with
    | Success (statement, (), _) ->
      statement |> Result.Ok
    | Failure (message, _, _) ->
      message |> Result.Error

module Say =
  let hello name =
    printfn "Hello %s" name
