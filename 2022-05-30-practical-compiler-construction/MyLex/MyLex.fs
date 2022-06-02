module MyLex

// -----------------------------------------------
// Parse
// -----------------------------------------------

exception ParseTermException of msg: string * index: int
exception ParseLexerException of msg: string * row: int * column: int

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Term =
  | String of string
  | AnyOf of byte []
  | NoneOf of byte []
  | Rep of Term
  | Rep1 of Term

let private occursAt (infix: string) (text: string) i =
  i + infix.Length <= text.Length
  && (infix.Length = 0 || text.[i] = infix.[0])
  && text.[i .. i + infix.Length - 1] = infix

let private unescape i (c: char) =
  match c with
  | '"' -> '\"'
  | '\\' -> '\\'
  | 'r' -> '\r'
  | 'n' -> '\n'
  | _ -> raise (ParseTermException("Invalid escape sequence", i))

let private parseStringLiteral (text: string) i =
  let rec go acc i =
    if i < text.Length then
      match text.[i] with
      | '\\' ->
        if i + 1 < text.Length then
          let c = unescape i text.[i + 1]
          go (string c :: acc) (i + 2)
        else
          raise (ParseTermException("Invalid escape sequence", i))

      | '\r'
      | '\n'
      | '"' -> acc, i + 1

      | _ ->
        let rec gogo i =
          let c =
            if i < text.Length then
              text.[i]
            else
              '\x00'

          match c with
          | '\\'
          | '"'
          | '\r'
          | '\n' -> i
          | _ -> gogo (i + 1)

        let r = gogo (i + 1)
        go (text.[i .. r - 1] :: acc) r
    else
      raise (ParseTermException("Expected end of string", i))

  assert (text.[i] = '"')
  let acc, i = go [] (i + 1)
  Term.String(String.concat "" (List.rev acc)), i

let private parseChars (text: string) i =
  assert (text.[i] = '[')

  let rec go acc i =
    if i < text.Length then
      match text.[i] with
      | ']' -> acc, i + 1

      | '\\' when i + 1 < text.Length -> go ([ byte (unescape i text.[i + 1]) ] :: acc) (i + 1)
      | '\\' -> raise (ParseTermException("Invalid escape sequence", i))

      | c when i + 2 < text.Length && text.[i + 1] = '-' ->
        let l = byte c
        let r = byte text.[i + 2]

        if l > r then
          raise (ParseTermException("Invalid char range", i))

        go
          (List.init (int (r - l)) (fun i -> l + byte i)
           :: acc)
          (i + 3)

      | c -> go ([ byte c ] :: acc) (i + 1)
    else
      raise (ParseTermException("Expected ']'", i))

  let exclude, i =
    if i + 1 < text.Length && text.[i + 1] = '^' then
      true, i + 1
    else
      false, i

  let acc, i = go [] (i + 1)

  let term =
    let chars = Array.ofList (List.concat acc)

    if exclude then
      Term.NoneOf chars
    else
      Term.AnyOf chars

  term, i

let private parseAtomicTerm (text: string) (i: int) =
  match text.[i] with
  | '"' -> parseStringLiteral text i
  | '[' -> parseChars text i

  | _ -> raise (ParseTermException("Expected '\"' or '['.", i))

let private parseRepTerm (text: string) (i: int) =
  let t, i = parseAtomicTerm text i

  if i < text.Length then
    match text.[i] with
    | '+' -> Term.Rep1 t, i + 1
    | '*' -> Term.Rep t, i + 1
    | _ -> t, i
  else
    t, i

let private parseTerm (text: string) i =
  let rec skipSpaces i =
    if i < text.Length && text.[i] = ' ' then
      skipSpaces (i + 1)
    else
      i

  let rec go acc i =
    let i = skipSpaces i

    if i < text.Length then
      let term, i = parseRepTerm text i
      go (term :: acc) i
    else
      List.rev acc

  go [] i

let parseLexer (text: string) =
  let lines =
    let text =
      if text.Contains('\r') then
        text.Replace("\r\n", "\n")
      else
        text

    text.Split('\n')

  lines
  |> Seq.mapi (fun i line -> i, line)
  |> Seq.filter (fun (_, (line: string)) -> line.Length <> 0)
  |> Seq.map (fun (row, line) ->
    let k = line.IndexOf(' ')

    if k < 0 then
      raise (ParseLexerException("Expected rule", row, 0))

    let name = line.[.. k - 1]
    printfn "%s: term '%s'" name line.[k + 1 ..]

    let term =
      try
        parseTerm line (k + 1)
      with
      | (ParseTermException (msg, column)) -> raise (ParseLexerException(msg, row, column))

    name, term)
  |> Seq.toList
