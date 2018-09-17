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

  let parseFlowValue tokens =
    match tokens with
    | TomlToken.Int value :: tokens ->
      Some (TomlValue.Int value), tokens
    | TomlToken.String value :: tokens ->
      Some (TomlValue.String value), tokens
    | _ ->
      None, tokens

  let parseBindings prefix acc tokens =
    match parsePath prefix tokens with
    | Some path, TomlToken.Eq :: tokens ->
      match parseFlowValue tokens with
      | Some value, tokens ->
        parseBindings prefix ((path, value) :: acc) tokens
      | None, tokens ->
        parseError "Expected a value just after '=' of binding." tokens
    | Some _, tokens ->
      parseError "Expected '=' just after key of binding." tokens
    | None, tokens ->
      acc, tokens

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

  let parse (tokens: list<TomlToken>): TomlTable =
    let bindings, tokens = parseBindings [] [] tokens
    let bindings = parseTableBindings bindings tokens
    bindings |> build

  let build (bindings: (string list * TomlValue) list): TomlTable =
    let deprefix bindings =
      bindings
      |> Seq.map (fun (path, value) ->
        let path = path |> List.truncate (List.length path - 1)
        (path, value)
      )
    let rec go (bindings: (string list * TomlValue) list) =
      let bindings =
        bindings
        |> Seq.groupBy (fun (path, _) -> List.last path)
        |> Seq.map
          (fun (key, bindings) ->
            let bindings = deprefix bindings |> Seq.toList
            match bindings with
            | [[], value] ->
              (key, value)
            | bindings ->
              let t = go bindings |> TomlValue.Table
              (key, t)
          )
        |> Seq.toList
      TomlTable bindings
    bindings |> List.rev |> go
