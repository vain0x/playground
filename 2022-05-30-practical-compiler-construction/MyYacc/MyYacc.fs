module MyYacc

module MyLex = MyLex

// -----------------------------------------------
// Parse Grammar
// -----------------------------------------------

exception ParseGrammarException of msg: string * index: int

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private ParsedTerm =
  | Token of string
  | Node of string

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private ParsedBranch =
  { Terms: ParsedTerm list
    NameOpt: string option
    PrecOpt: string option }

type private ParsedRule = string * ParsedBranch list

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Prec =
  | Left
  | NonAssoc

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private Grammar =
  { Rules: ParsedRule list
    StartOpt: string option
    Prec: (Prec * string list) list }

let private occursAt (infix: string) (text: string) i =
  i + infix.Length <= text.Length
  && (infix.Length = 0 || text.[i] = infix.[0])
  && text.[i .. i + infix.Length - 1] = infix

let private skipSpaces (text: string) i =
  let space c = c = ' ' || c = '\r' || c = '\n'

  let rec go i =
    if i < text.Length && space text.[i] then
      go (i + 1)
    else if occursAt "/*" text i then
      let j = text.IndexOf("*/", i + 2)

      if j >= i + 2 then
        j + 2
      else
        text.Length
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
      let i = skipSpaces text i

      if i < text.Length && isAlphabetic text.[i] then
        let name, i = expectIdent text i

        let term =
          (if System.Char.IsUpper(name.[0]) then
             ParsedTerm.Token
           else
             ParsedTerm.Node)
            name

        go (term :: acc) i
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

  let b: ParsedBranch =
    { Terms = terms
      NameOpt = nameOpt
      PrecOpt = precOpt }

  b, i

let private parseRule (text: string) i =
  let name, i = expectIdent text i
  let i = expectText ":" text (skipSpaces text i)

  let branches, i =
    let rec go acc i =
      let i = skipSpaces text i

      if i < text.Length then
        let b, i = parseBranch text i
        let acc = b :: acc
        let i = skipSpaces text i

        if i < text.Length && text.[i] = '|' then
          go acc (i + 1)
        else
          acc, i
      else
        acc, i

    go [] i

  let rule: ParsedRule = name, List.rev branches

  let i = expectText ";" text (skipSpaces text i)
  rule, i

type private Acc =
  { PrecAcc: (Prec * string list) list
    RuleAcc: (bool * ParsedRule) list }

let private emptyAcc: Acc = { PrecAcc = []; RuleAcc = [] }

let private addRule item (acc: Acc) =
  { acc with RuleAcc = item :: acc.RuleAcc }

let private addPrec prec tokens (acc: Acc) =
  { acc with PrecAcc = (prec, tokens) :: acc.PrecAcc }

let private parseDirective acc (text: string) i =
  let onPrec prec i =
    let line, i = eatLine text i
    let tokens = line.Split(' ') |> List.ofArray
    addPrec prec tokens acc, i

  let onRule beingStart i =
    let rule, i = parseRule text i
    addRule (beingStart, rule) acc, i

  match text.[i] with
  | '%' when i + 1 < text.Length ->
    let directiveIndex = i
    let name, i = expectIdent text (i + 1)
    let i = skipSpaces text i

    match name with
    | "nonassoc" -> onPrec Prec.NonAssoc i
    | "left" -> onPrec Prec.Left i
    | "start" -> onRule true i

    | _ ->
      ParseGrammarException("Unknown directive", directiveIndex)
      |> raise

  | c when 'a' <= c && c <= 'z' -> onRule false i

  | _ ->
    ParseGrammarException("Expected directive", i)
    |> raise

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private Term =
  | Token of name: string
  | Node of int * string

type private Branch = int * string * (int * Prec) * Term list
type private Rule = int * string * Branch list

// see grammar.txt for example
let private parseGrammar (text: string) : Rule list =
  let rec go acc i =
    if i < text.Length then
      let acc, i = parseDirective acc text i
      go acc (skipSpaces text i)
    else
      acc

  let acc = go emptyAcc (skipSpaces text 0)

  let parsedRules = List.rev acc.RuleAcc

  let _, precMap =
    let ruleCount = List.length parsedRules

    List.rev acc.PrecAcc
    |> List.fold
         (fun (i, acc) (prec, tokens) ->
           let acc =
             tokens
             |> List.fold (fun acc token -> acc |> Map.add token (i, prec)) acc

           i + 1, acc)
         (ruleCount, Map.empty)

  let rules =
    let ruleMap =
      parsedRules
      |> List.mapi (fun i (_, (name, _)) -> name, i)
      |> Map.ofList

    parsedRules
    |> List.mapi (fun i (_, (name, branches)) ->
      let branches: Branch list =
        branches
        |> List.mapi (fun i (b: ParsedBranch) ->
          let name =
            match b.NameOpt with
            | Some name -> name
            | None -> name + "_" + string (i + 1)

          let prec =
            match b.PrecOpt with
            | Some prec ->
              match precMap |> Map.tryFind prec with
              | Some prec -> prec
              | None -> failwithf "Unknown token specified by 'prec' (%s)" prec

            | None ->
              b.Terms
              |> List.tryPick (fun t ->
                match t with
                | ParsedTerm.Token t -> precMap |> Map.tryFind t
                | _ -> None)
              |> Option.defaultValue (i, Prec.NonAssoc)

          let terms =
            b.Terms
            |> List.map (fun t ->
              match t with
              | ParsedTerm.Token t -> Term.Token t
              | ParsedTerm.Node node ->
                match ruleMap |> Map.tryFind node with
                | Some i -> Term.Node(i, node)
                | None -> failwithf "Unknown node '%s'" name)

          i, name, prec, terms)

      i, name, branches)

  rules

// -----------------------------------------------
// Nullable
// -----------------------------------------------

type private NodeId = int
type private NullableSet = Set<int>

let private computeNullableSet (rules: Rule list) : NullableSet =
  let nullableSet, rules =
    let isNode t =
      match t with
      | Term.Node _ -> true
      | _ -> false

    let nodeOnly b =
      let _, _, _, terms = b
      terms |> List.forall isNode

    rules
    |> List.fold
         (fun (nullableSet, ruleAcc) rule ->
           let i, _, branches = rule

           if List.isEmpty branches then
             Set.add i nullableSet, ruleAcc
           else if List.exists nodeOnly branches then
             nullableSet, rule :: ruleAcc
           else
             nullableSet, ruleAcc)
         (Set.empty, [])

  let update nullableSet rules =
    rules
    |> List.fold
         (fun (modified, nullableSet, ruleAcc) rule ->
           let i, _, branches = rule

           let nullable =
             branches
             |> List.exists (fun (_, _, _, terms) ->
               terms
               |> List.forall (fun t ->
                 match t with
                 | Term.Node (i, _) -> Set.contains i nullableSet
                 | Term.Token _ -> false))

           if nullable then
             true, Set.add i nullableSet, ruleAcc
           else
             modified, nullableSet, rule :: ruleAcc)
         (false, nullableSet, [])

  let rec makeClosure nullableSet rules =
    let modified, nullableSet, rules = update nullableSet rules

    if modified then
      makeClosure nullableSet rules
    else
      nullableSet

  makeClosure nullableSet rules

// -----------------------------------------------
// First
// -----------------------------------------------

type private FirstSet = Map<NodeId, Set<string>>

let private computeFirstSet (nullableSet: NullableSet) (rules: Rule list) : FirstSet =
  let isNullable i = nullableSet |> Set.contains i

  let termsToFirst firstSet terms =
    let rec go acc terms =
      match terms with
      | [] -> acc
      | Term.Token t :: _ -> acc |> Set.add t

      | Term.Node (i, _) :: terms ->
        let acc =
          firstSet
          |> Map.tryFind i
          |> Option.defaultValue Set.empty
          |> Set.union acc

        if isNullable i then
          go acc terms
        else
          acc

    go Set.empty terms

  let update firstSet =
    rules
    |> List.fold
         (fun (modified, firstSet) rule ->
           let i, _, branches = rule

           let prev =
             firstSet
             |> Map.tryFind i
             |> Option.defaultValue Set.empty

           let next =
             branches
             |> List.map (fun (_, _, _, terms) -> termsToFirst firstSet terms)
             |> Set.unionMany

           if prev <> next then
             true, Map.add i next firstSet
           else
             modified, firstSet)
         (false, firstSet)

  let rec makeClosure firstSet =
    let modified, firstSet = update firstSet

    if modified then
      makeClosure firstSet
    else
      firstSet

  makeClosure Map.empty

let dump text =
  let g = parseGrammar text

  let nullableSet = computeNullableSet g
  let firstSet = computeFirstSet nullableSet g
  // let followSet = computeFollowSet nullableSet firstSet g

  let ruleMap =
    g
    |> List.map (fun (i, name, _) -> i, name)
    |> Map.ofList

  let ruleName i = ruleMap |> Map.find i

  printfn
    "nullable %s"
    (g
     |> List.choose (fun (i, name, _) ->
       if nullableSet |> Set.contains i then
         Some name
       else
         None)
     |> String.concat ", ")

  printfn "first:"

  for i, name, _ in g do
    let tokens =
      firstSet
      |> Map.tryFind i
      |> Option.defaultValue Set.empty
      |> Set.toList
      |> List.sort
      |> String.concat ", "

    printfn "  %s: %s" name tokens

// printfn "follow:"

// for i, name, _ in g do
//   let tokens =
//     followSet
//     |> Map.tryFind i
//     |> Option.defaultValue Set.empty
//     |> Set.toList
//     |> List.sort
//     |> String.concat ", "

//   printfn "  %s: %s" name tokens
