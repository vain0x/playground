module rec TomlHummer.Parsing
  open System

  let parseError message tokens =
    failwithf "%s Near %A" message (tokens |> List.truncate 5)

  let (|Key|) tokens =
    match tokens with
    | TomlToken.Ident ident :: tokens ->
      Some ident, tokens
    | TomlToken.String str :: tokens ->
      Some str, tokens
    | _ ->
      None, tokens

  let (|Flow|) tokens =
    match tokens with
    | TomlToken.Int value :: tokens ->
      Some (TomlValue.Int value), tokens
    | TomlToken.String value :: tokens ->
      Some (TomlValue.String value), tokens
    | _ ->
      None, tokens

  let parseBindings acc tokens =
    match tokens with
    | Key (Some key, TomlToken.Eq :: Flow (Some value, tokens)) ->
      parseBindings ((key, value) :: acc) tokens
    | Key (Some _, tokens) ->
      match tokens with
      | TomlToken.Eq :: _ ->
        parseError "Expected a value just after '=' of binding." tokens
      | _ ->
        parseError "Expected '=' just after key of binding." tokens
    | TomlToken.BracketL :: _
    | TomlToken.BracketLL :: _
    | TomlToken.Eof :: _ ->
      acc, tokens
    | _ ->
      parseError "Expected a binding for current table, a table, an array-of-table, or end of input." tokens

  let parseTable tokens =
    let acc, tokens = parseBindings [] tokens
    let t = acc |> List.rev |> TomlTable |> TomlValue.Table
    t, tokens

  let parseTableBindings acc tokens =
    match tokens with
    | TomlToken.BracketL :: Key (Some key, TomlToken.BracketR :: tokens) ->
      let t, tokens = parseTable tokens
      parseTableBindings ((key, t) :: acc) tokens
    | TomlToken.BracketLL :: Key (Some _key, TomlToken.BracketRR :: _tokens) ->
      failwith "array of tables"
    | [TomlToken.Eof] ->
      acc
    | _ ->
      parseError "Expected a table, an array-of-table, or end of input." tokens

  let parse (tokens: list<TomlToken>): TomlTable =
    let bindings, tokens = parseBindings [] tokens
    let bindings = parseTableBindings bindings tokens
    bindings |> List.rev |> TomlTable
