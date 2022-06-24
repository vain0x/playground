module MyLex

type private Dictionary<'K, 'T> = System.Collections.Generic.Dictionary<'K, 'T>

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

      | '\\' when i + 1 < text.Length -> go ([ byte (unescape i text.[i + 1]) ] :: acc) (i + 2)
      | '\\' -> raise (ParseTermException("Invalid escape sequence", i))

      | c when i + 2 < text.Length && text.[i + 1] = '-' ->
        let l = byte c
        let r = byte text.[i + 2]

        if l > r then
          raise (ParseTermException("Invalid char range", i))

        go
          (List.init (int (r - l) + 1) (fun i -> l + byte i)
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

// see lex.txt for example
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

    let term =
      try
        parseTerm line (k + 1)
      with
      | (ParseTermException (msg, column)) -> raise (ParseLexerException(msg, row, column))

    name, term)
  |> Seq.toList

// -----------------------------------------------
// NTerm
// -----------------------------------------------

// Term -> NTerm

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private NTerm =
  | AnyOf of byte []
  | Conj of NTerm * NTerm
  | Rep of NTerm

let rec private lower term =
  match term with
  | Term.String s ->
    let rec go (l: int) r =
      assert (l < r)

      if r = l + 1 then
        NTerm.AnyOf [| byte s.[l] |]
      else
        let m = (l + r) / 2
        NTerm.Conj(go l m, go m r)

    assert (s.Length <> 0)
    go 0 s.Length

  | Term.AnyOf chars -> NTerm.AnyOf chars

  | Term.NoneOf chars ->
    NTerm.AnyOf [| for c in 1uy .. 255uy do
                     if Array.contains c chars |> not then c |]

  | Term.Rep t -> NTerm.Rep(lower t)

  | Term.Rep1 t ->
    let t = lower t
    NTerm.Conj(t, NTerm.Rep t)

let private lowerTerms terms =
  let rec go acc terms =
    match terms with
    | term :: terms -> go (NTerm.Conj(acc, lower term)) terms
    | [] -> acc

  match terms with
  | term :: terms -> go (lower term) terms
  | _ -> failwith "NEVER"

// -----------------------------------------------
// NFA
// -----------------------------------------------

let private getMulti key map =
  Map.tryFind key map |> Option.defaultValue []

let private addMulti key value map =
  Map.add key (value :: (getMulti key map)) map

/// Label of ε-transition.
let private eps = 0uy

let generateNfa (rules: (string * Term list) list) =
  let fresh (trans, last) = last + 1, (trans, last + 1)
  let connect (c: byte) (u: int) (v: int) (trans, last) = addMulti (u, c) v trans, last

  let rec go (u: int) b term =
    match term with
    | NTerm.AnyOf chars ->
      let v, b = fresh b
      let b = chars |> Array.fold (fun b c -> connect c u v b) b
      v, b

    | NTerm.Conj (l, r) ->
      let v, b = go u b l
      go v b r

    | NTerm.Rep t ->
      //      ε
      // ->u ---> ((v)) <------+
      //            |          | ε
      //            |          |
      //            +-> [t] -> w

      let v, b = fresh b
      let w, b = go v b t
      let b = b |> connect eps u v |> connect eps w v
      v, b

  let u = 1 // start state
  let b = Map.empty, u

  let b, accepts =
    rules
    |> List.fold
         (fun (b, accepts) (name, terms) ->
           // ->u --[terms]--> ((v))
           let v, b = go u b (lowerTerms terms)

           // Remember accept state and its name.
           let accepts = accepts |> Map.add v name

           b, accepts)
         (b, Map.empty)

  let trans, _ = b
  u, trans, accepts

// -----------------------------------------------
// NFA
// -----------------------------------------------

let private computeClosure (s: Set<int>) trans =
  let enhance s =
    s
    |> Set.fold
         (fun (modified, s) u ->
           getMulti (u, eps) trans
           |> List.fold
                (fun (modified, s) v ->
                  if Set.contains v s |> not then
                    true, Set.add v s
                  else
                    modified, s)
                (modified, s))
         (false, s)

  let rec go s =
    let modified, s = enhance s
    if modified then go s else s

  go s

let private computeDfaEdge (d: Set<int>) (c: byte) trans =
  d
  |> Set.fold
       (fun x s ->
         let e = getMulti (s, c) trans |> Set.ofList
         Set.union x (computeClosure e trans))
       Set.empty

let emulateNfa (input: string) nfa : string option =
  let u, trans, accepts = nfa

  let d =
    input.ToCharArray()
    |> Array.fold
         (fun (d: Set<int>) (c: char) ->
           let c = byte c
           computeDfaEdge d c trans)
         (Set.singleton u)

  d
  |> Set.fold
       (fun opt v ->
         match opt with
         | Some _ -> opt
         | None ->
           match accepts |> Map.tryFind v with
           | (Some _) as opt -> opt
           | None -> None)
       None

exception TokenizeException of index: int

let tokenizeWithNfa (input: string) nfa : (string * int) list =
  let u, trans, accepts = nfa

  let setRev = ResizeArray()
  let setMemo = Dictionary<Set<int>, int>()
  let edgeMemo = Dictionary<int * byte, int>()
  let acceptMemo = ResizeArray()

  let internSet (d: Set<int>) =
    match setMemo.TryGetValue(d) with
    | true, it -> it
    | false, _ ->
      let n = setRev.Count
      setRev.Add(d)
      setMemo.Add(d, n)
      eprintfn "set %d -> %A" n d
      n

  // state -> labelOpt
  let accept (d: int) : string option =
    if d >= acceptMemo.Count then
      for e in acceptMemo.Count .. d do
        let labelOpt =
          setRev.[e]
          |> Set.fold
               (fun opt v ->
                 match opt with
                 | Some _ -> opt
                 | None -> accepts |> Map.tryFind v)
               None

        eprintfn "accept %d -> %A" e labelOpt
        acceptMemo.Add(labelOpt)

    acceptMemo.[d]

  // state -> c -> nextState
  let getDfaEdge (d: int) (c: byte) : int =
    match edgeMemo.TryGetValue((d, c)) with
    | true, it -> it
    | false, _ ->
      let e = computeDfaEdge setRev.[d] c trans
      let n = internSet e
      edgeMemo.Add((d, c), n)
      eprintfn "edge %d,%d -> %A" d c n
      n

  let emptyState = internSet Set.empty
  assert (emptyState = 0)
  let initialState = internSet (Set.singleton u)
  assert (initialState = 1)

  let input = input.ToCharArray()
  let mutable tick = 0

  let rec tokenizeLoop acc l =
    if l < input.Length then
      let rec go last d i =
        tick <- tick + 1

        if d <> emptyState && i < input.Length then
          let d = getDfaEdge d (byte input.[i])

          let last =
            match accept d with
            | Some label -> Some(label, i + 1)
            | None -> last

          if d <> emptyState && i + 1 < input.Length then
            eprintfn "gogo d:%d i:%d -> %A" d i last

          go last d (i + 1)
        else
          last

      match go None initialState l with
      | Some (label, r) when l < r -> tokenizeLoop ((label, r - l) :: acc) r
      | _ -> raise (TokenizeException l)
    else
      List.rev acc

  let result = tokenizeLoop [] 0

  eprintfn
    "stats set:%d,%d edge:%d accept:%d tick:%d\n  cost:%d"
    setRev.Count
    setMemo.Count
    edgeMemo.Count
    acceptMemo.Count
    tick
    (setRev.Count
     + setMemo.Count
     + edgeMemo.Count
     + acceptMemo.Count
     + tick)

  result
