module MyYacc

module MyLex = MyLex

// -----------------------------------------------
// Parse Grammar
// -----------------------------------------------

exception ParseGrammarException of msg: string * index: int

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private Term =
  | Token of string
  | Node of string

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private Branch =
  { Terms: Term list
    NameOpt: string option
    PrecOpt: string option }

type private Rule = string * Branch list

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private Prec =
  | Left
  | NonAssoc

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private Grammar =
  { Rules: Rule list
    StartOpt: string option
    Prec: (Prec * string list) list }

let private addRule rule beingStart (g: Grammar) =
  { g with
      Rules = rule :: g.Rules
      StartOpt =
        if beingStart then
          let name, _ = rule
          Some name
        else
          g.StartOpt }

let private addPrec prec tokens (g: Grammar) =
  { g with Prec = (prec, tokens) :: g.Prec }

let private occursAt (infix: string) (text: string) i =
  i + infix.Length <= text.Length
  && (infix.Length = 0 || text.[i] = infix.[0])
  && text.[i .. i + infix.Length - 1] = infix

let private skipSpaces (text: string) i =
  let space c = c = ' ' || c = '\r' || c = '\n'

  let rec go i =
    if i < text.Length && space text.[i] then
      go (i + 1)
    else
      i

  go i

let private eatLine (text: string) i =
  let found = text.IndexOfAny([| '\r'; '\n' |], i)

  let r =
    if found >= i then
      found
    else
      text.Length

  text.[i .. r - 1], r

let private isDigit (c: char) = '0' <= c && c <= '9'

let private isAlphabetic (c: char) =
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

let private expectIdent (text: string) i =
  let okay (c: char) = isAlphabetic c || isDigit c || c = '_'

  let rec go i =
    if i < text.Length && okay text.[i] then
      go (i + 1)
    else
      i

  if i < text.Length
     && (isAlphabetic text.[i] || text.[i] = '_') then
    let r = go (i + 1)
    text.[i .. r - 1], r
  else
    ParseGrammarException("Expected an identifier", i)
    |> raise

let private expectText (infix: string) (text: string) i =
  if occursAt infix text i then
    i + infix.Length
  else
    ParseGrammarException($"Expected '{infix}'", i)
    |> raise

let private parseBranch (text: string) i =
  let terms, i =
    let rec go acc i =
      if i < text.Length && isAlphabetic text.[i] then
        let name, i = expectIdent text i

        let term =
          (if name.[0] <= 'Z' then
             Term.Node
           else
             Term.Token)
            name

        go (term :: acc) (skipSpaces text i)
      else
        List.rev acc, i

    go [] i

  let nameOpt, i =
    if i < text.Length && occursAt "%name" text i then
      let i = skipSpaces text (i + "%name".Length)
      let name, i = expectIdent text i
      Some name, i
    else
      None, i

  let i = skipSpaces text i

  let precOpt, i =
    if i < text.Length && occursAt "%prec" text i then
      let i = skipSpaces text (i + "%prec".Length)
      let name, i = expectIdent text i
      Some name, i
    else
      None, i

  let b: Branch =
    { Terms = terms
      NameOpt = nameOpt
      PrecOpt = precOpt }

  b, i

let private parseRule (text: string) i =
  let name, i = expectIdent text i
  let i = expectText ":" text (skipSpaces text i)

  let branches, i =
    let rec go acc i =
      if i < text.Length then
        let i = skipSpaces text i
        let b, i = parseBranch text i
        let acc = b :: acc

        if i < text.Length && text.[i] = '|' then
          go acc (i + 1)
        else
          acc, i
      else
        acc, i

    go [] i

  let rule: Rule = name, branches

  let i = expectText ";" text (skipSpaces text i)
  rule, i

let private parseDirective acc (text: string) i =
  let onPrec prec i =
    let line, i = eatLine text i
    let tokens = line.Split(' ') |> List.ofArray
    acc |> addPrec prec tokens, i

  let onRule start i =
    let rule, i = parseRule text i
    addRule rule start acc, i

  match text.[i] with
  | '%' when i + 1 < text.Length ->
    let j = i
    let name, i = expectIdent text i
    let i = skipSpaces text i

    match name with
    | "nonassoc" -> onPrec Prec.NonAssoc i
    | "left" -> onPrec Prec.Left i
    | "start" -> onRule true i

    | _ ->
      ParseGrammarException("Unknown directive", j)
      |> raise

  | c when 'a' <= c && c <= 'z' -> onRule false i

  | _ ->
    ParseGrammarException("Expected directive", i)
    |> raise

// see grammar.txt for example
let private parseGrammar (text: string) =
  let rec go acc i =
    let i = skipSpaces text i

    if i < text.Length then
      let acc, i = parseDirective acc text i
      go acc i
    else
      acc

  let g: Grammar =
    { Rules = []
      StartOpt = None
      Prec = [] }

  go g 0
