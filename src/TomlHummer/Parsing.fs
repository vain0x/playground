module rec TomlHummer.Parsing
  open System

  [<RequireQualifiedAccess>]
  type Key =
    | Scalar
      of string
    | Array
      of string
    | Table
      of string

  let toScalarKey key =
    match key with
    | Key.Scalar _ -> key
    | Key.Array key
    | Key.Table key -> Key.Scalar key

  let toScalarPath path =
    match path with
    | [] -> []
    | key :: path ->
      toScalarKey key :: path

  let toArrayPath path =
    match path with
    | [] -> []
    | (Key.Scalar key | Key.Array key | Key.Table key) :: path ->
      Key.Array key :: path

  let parseError message tokens =
    failwithf "%s Near %A" message (tokens |> List.truncate 5)

  let parseKey tokens =
    match tokens with
    | TomlToken.Ident ident :: tokens ->
      Some (Key.Table ident), tokens
    | TomlToken.String str :: tokens ->
      Some (Key.Table str), tokens
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

  /// Parses ``[ value, ... ]``.
  let parseInlineArray prefix acc tokens =
    let rec go acc tokens =
      match tokens with
      | (TomlToken.BracketL | TomlToken.Comma) :: tokens ->
        let freshKey = Guid.NewGuid().ToString() |> Key.Table
        let path = freshKey :: toArrayPath prefix
        let acc, tokens = parseInlineValue path acc tokens
        go acc tokens
      | TomlToken.BracketR :: tokens ->
        acc, tokens
      | _ ->
        parseError "Expected ',' or ']' between items of array." tokens

    assert (tokens |> List.head = TomlToken.BracketL)
    go acc tokens

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
      (toScalarPath path, TomlValue.Int value) :: acc, tokens
    | TomlToken.String value :: tokens ->
      (toScalarPath path, TomlValue.String value) :: acc, tokens
    | TomlToken.BracketL :: _ ->
      parseInlineArray path acc tokens
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
      match parsePath [] tokens with
      | Some path, TomlToken.BracketRR :: tokens ->
        // Anonymous and fresh key to identify the table in build process.
        let freshKey = Guid.NewGuid().ToString() |> Key.Table
        let path = freshKey :: toArrayPath path
        let acc, tokens = parseBindings path acc tokens
        parseTableBindings acc tokens
      | Some _, tokens ->
        parseError "Expected ']]'." tokens
      | _ ->
        parseError "Expected a path." tokens
    | [TomlToken.Eof] ->
      acc
    | _ ->
      parseError "Expected a table, an array-of-table, or end of input." tokens

  /// Parses tokens as top-level toml code.
  let parse (tokens: list<TomlToken>): TomlTable =
    let bindings, tokens = parseBindings [] [] tokens
    let bindings = parseTableBindings bindings tokens
    bindings |> build

  let build (bindings: (Key list * TomlValue) list): TomlTable =
    let bindings =
      bindings |> List.map (fun (path, value) -> (List.rev path, value))
    let rec go (bindings: (Key list * TomlValue) seq) =
      let bindings =
        bindings
        |> Seq.map (fun (path, value) ->
          match path with
          | [] -> failwith "never"
          | key :: path -> key, (path, value)
        )
        |> Seq.groupBy fst
        |> Seq.map
          (fun (key, bindings) ->
            match key with
            | Key.Scalar key ->
              let _, (_, value) = bindings |> Seq.exactlyOne
              (key, value)
            | Key.Array key ->
              // Build recursively. Each binding is (GUID, array-item).
              let (TomlTable bindings) = bindings |> Seq.map snd |> go
              let a = bindings |> Seq.map snd |> Seq.toList |> TomlValue.Array
              (key, a)
            | Key.Table key ->
              let t = bindings |> Seq.map snd |> go |> TomlValue.Table
              (key, t)
          )
        |> Seq.toList
      TomlTable bindings
    bindings |> List.rev |> go
