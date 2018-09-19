module rec TomlHummer.Parsing
  open System

  let parseError message tokens =
    failwithf "%s Near %A" message (tokens |> List.truncate 5)

  let parseKey tokens =
    match tokens with
    | TomlToken.Ident ident :: tokens ->
      Some ident, tokens
    | TomlToken.String str :: tokens ->
      Some str, tokens
    | _ ->
      None, tokens

  let parsePath prefix tokens =
    let rec go path tokens =
      match tokens with
      | TomlToken.Dot :: tokens ->
        match parseKey tokens with
        | Some k, tokens ->
          go (k :: path) tokens
        | None, _ ->
          parseError "Expected an identifier or a string." tokens
      | _ ->
        path, tokens
    match parseKey tokens with
    | Some k, tokens ->
      let path, tokens = go (k :: prefix) tokens
      Some path, tokens
    | _ ->
      None, tokens

  /// Parses ``{ path = value, ... }``.
  let parseInlineBindings prefix acc tokens =
    let rec go acc tokens =
      match tokens with
      | (TomlToken.BraceL | TomlToken.Comma) :: tokens ->
        match parsePath prefix tokens with
        | Some path, TomlToken.Eq :: tokens ->
          let acc, tokens = parseInlineValue path acc tokens
          go acc tokens
        | Some _, tokens ->
          parseError "Expected '='." tokens
        | None, _ ->
          parseError "Expected a path." tokens
      | TomlToken.BraceR :: tokens ->
        acc, tokens
      | _ ->
        parseError "Expected 'path = value'." tokens

    assert (tokens |> List.head = TomlToken.BraceL)
    go acc tokens

  let parseInlineValue path acc tokens =
    match tokens with
    | TomlToken.Int value :: tokens ->
      (path, TomlValue.Int value) :: acc, tokens
    | TomlToken.String value :: tokens ->
      (path, TomlValue.String value) :: acc, tokens
    | TomlToken.BraceL :: _ ->
      parseInlineBindings path acc tokens
    | _ ->
      parseError "Expected a value just after '=' of binding." tokens

  /// Parses ``p = v \n p2 = v2 \n ...`` until next section.
  let parseBindings prefix acc tokens =
    match parsePath prefix tokens with
    | Some path, TomlToken.Eq :: tokens ->
      let acc, tokens = parseInlineValue path acc tokens
      parseBindings prefix acc tokens
    | Some _, tokens ->
      parseError "Expected '=' just after key of binding." tokens
    | None, tokens ->
      acc, tokens

  /// Parses ``[t] p = v ...`` until next section.
  let parseTableBindings acc tokens =
    match tokens with
    | TomlToken.BracketL :: tokens ->
      match parsePath [] tokens with
      | Some path, TomlToken.BracketR :: tokens ->
        let acc, tokens = parseBindings path acc tokens
        parseTableBindings acc tokens
      | Some _, tokens ->
        parseError "Expected ']'." tokens
      | _ ->
        parseError "Expected a path." tokens
    | TomlToken.BracketLL :: tokens ->
      failwith "array of tables"
    | [TomlToken.Eof] ->
      acc
    | _ ->
      parseError "Expected a table, an array-of-table, or end of input." tokens

  /// Parses tokens as top-level toml code.
  let parse (tokens: list<TomlToken>): TomlTable =
    let bindings, tokens = parseBindings [] [] tokens
    let bindings = parseTableBindings bindings tokens
    bindings |> build

  let build (bindings: (string list * TomlValue) list): TomlTable =
    let bindings =
      bindings |> List.map (fun (path, value) -> (List.rev path, value))
    let rec go (bindings: (string list * TomlValue) list) =
      let bindings =
        bindings
        |> Seq.groupBy (fun (path, _) -> List.head path)
        |> Seq.map
          (fun (key, bindings) ->
            match [for path, value in bindings -> List.tail path, value] with
            | [[], value] ->
              (key, value)
            | bindings ->
              let t = go bindings |> TomlValue.Table
              (key, t)
          )
        |> Seq.toList
      TomlTable bindings
    bindings |> List.rev |> go
