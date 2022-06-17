module MyYacc

module MyLex = MyLex

type private Dictionary<'K, 'T> = System.Collections.Generic.Dictionary<'K, 'T>

let private unwrapOr alt opt = Option.defaultValue alt opt

let private getMultiset key map =
  Map.tryFind key map
  |> Option.defaultValue Set.empty

let private addMultiset key value map =
  Map.add key (Set.add value (getMultiset key map)) map

// -----------------------------------------------
// Types
// -----------------------------------------------

/// 結合性
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Assoc =
  | Left
  | NonAssoc

/// 開始規則ならtrue
type private BeingStart = bool

/// トークンの名前
type private TokenText = string
/// ノードの名前
type private NodeText = string

/// 項をインターン化したID
type private TermId = int
type private TokenId = TermId
type private NodeId = TermId

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private Term =
  | Token of TermId * name: TokenText
  | Node of TermId * name: TokenText

type private TermMap = Map<TermId, Term>
type private TokenMemo = Map<TokenText, TokenId>

/// branchId, name, precedence, terms
type private Branch = int * string * (int * Assoc) * Term list
/// nodeId, name, branches
type private Rule = NodeId * Branch list

module private Term =
  let id term =
    match term with
    | Term.Token (i, _) -> i
    | Term.Node (i, _) -> i

  let toString term : string =
    match term with
    | Term.Token (_, t) -> t
    | Term.Node (_, t) -> t

let private Eof: TokenId = 0

// -----------------------------------------------
// Parse Grammar
// -----------------------------------------------

exception ParseGrammarException of msg: string * index: int

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private ParsedTerm =
  | Token of TokenText
  | Node of NodeText

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private ParsedBranch =
  { Terms: ParsedTerm list
    NameOpt: string option
    PrecOpt: TokenText option }

type private ParsedRule = NodeText * ParsedBranch list

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

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private Acc =
  { PrecAcc: (Assoc * TokenText list) list
    RuleAcc: (BeingStart * ParsedRule) list }

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
    | "nonassoc" -> onPrec Assoc.NonAssoc i
    | "left" -> onPrec Assoc.Left i
    | "start" -> onRule true i

    | _ ->
      ParseGrammarException("Unknown directive", directiveIndex)
      |> raise

  | c when 'a' <= c && c <= 'z' -> onRule false i

  | _ ->
    ParseGrammarException("Expected directive", i)
    |> raise

// see grammar.txt for example
let private parseGrammar (text: string) =
  let rec go acc i =
    if i < text.Length then
      let acc, i = parseDirective acc text i
      go acc (skipSpaces text i)
    else
      acc

  go emptyAcc (skipSpaces text 0)

// -----------------------------------------------
// Lower
// -----------------------------------------------

/// パースの結果を中間表現に変形する
let private lower (parsedAcc: Acc) =
  let parsedPrec = List.rev parsedAcc.PrecAcc
  let parsedRules = List.rev parsedAcc.RuleAcc

  // 項の名前にIDを割り振る
  let termArray = ResizeArray([ Term.Token(Eof, "$") ])
  let termCount = 1

  let termCount, tokenMemo, nodeMemo =
    let BeingToken = false
    let BeingNode = true

    let acc =
      parsedPrec
      |> List.fold
           (fun acc (_, (ts: TokenText list)) ->
             ts
             |> List.fold (fun acc t -> Set.add (BeingToken, t) acc) acc)
           Set.empty

    let acc =
      parsedRules
      |> List.fold
           (fun acc (_, rule) ->
             let name, branches = rule
             let acc = Set.add (BeingNode, name) acc

             branches
             |> List.fold
                  (fun acc (b: ParsedBranch) ->
                    let acc =
                      match b.PrecOpt with
                      | Some t -> Set.add (BeingToken, t) acc
                      | None -> acc

                    b.Terms
                    |> List.fold
                         (fun acc term ->
                           match term with
                           | ParsedTerm.Token t -> Set.add (BeingToken, t) acc
                           | ParsedTerm.Node t -> Set.add (BeingNode, t) acc)
                         acc)
                  acc)
           acc

    acc
    |> Set.fold
         (fun ((i: TermId), tMemo, nMemo) (beingNode, t) ->
           let term, tMemo, nMemo =
             if beingNode then
               eprintfn "node %d:%s" i t
               Term.Node(i, t), tMemo, Map.add t i nMemo
             else
               eprintfn "token %d:%s" i t
               Term.Token(i, t), Map.add t i tMemo, nMemo

           termArray.Add(term)
           i + 1, tMemo, nMemo)
         (termCount, Map.empty, Map.empty)

  let internToken (t: string) : TokenId = Map.find t tokenMemo
  let internNode (t: NodeText) : NodeId = Map.find t nodeMemo

  // 優先度指定に書かれたトークンのインターン化と、それらのトークンへの優先度の割り振りを行う。
  // これらの優先度はすべての項IDより大きい値を設定することで暗黙的な優先順位より強くする
  let _, precMap =
    parsedPrec
    |> List.fold
         (fun (p, acc) (prec, ts) ->
           let p = p + 1

           let acc =
             ts
             |> List.fold (fun acc t -> acc |> Map.add (internToken t) (p, prec)) acc

           p, acc)
         (termCount, Map.empty)

  // ルールに書かれた項をインターン化する。
  // ブランチへの名前の割り振り、優先度の決定などの導出計算も行う
  let rules: Rule list =
    let ruleMemo =
      parsedRules
      |> List.map (fun (_, (name, _)) -> name, internNode name)
      |> Map.ofList

    parsedRules
    |> List.map (fun (_, (ruleName, branches)) ->
      let branches: Branch list =
        branches
        |> List.mapi (fun bi (b: ParsedBranch) ->
          let name =
            match b.NameOpt with
            | Some name -> name
            | None -> ruleName + "_" + string (bi + 1)

          let prec =
            match b.PrecOpt with
            | Some prec ->
              match precMap |> Map.tryFind (internToken prec) with
              | Some prec -> prec
              | None -> failwithf "Unknown token specified by '%%prec' (%s)" prec

            | None ->
              b.Terms
              |> List.tryPick (fun t ->
                match t with
                | ParsedTerm.Token t -> precMap |> Map.tryFind (internToken t)
                | _ -> None)
              |> unwrapOr (bi, Assoc.NonAssoc)

          let terms =
            b.Terms
            |> List.map (fun t ->
              match t with
              | ParsedTerm.Token t -> Term.Token(internToken t, t)
              | ParsedTerm.Node t ->
                match ruleMemo |> Map.tryFind t with
                | Some i -> Term.Node(i, t)
                | None -> failwithf "Undefined node '%s'" name)

          bi, name, prec, terms)

      internNode ruleName, branches)

  let startRule =
    let _, (ruleName, _) =
      parsedRules
      |> List.tryFind (fun (start, _) -> start)
      |> Option.orElseWith (fun () -> List.tryLast parsedRules)
      |> Option.defaultWith (fun () -> failwith "No rules")

    internNode ruleName

  termArray.ToArray(), tokenMemo, nodeMemo, startRule, rules

// -----------------------------------------------
// Nullable
// -----------------------------------------------

type private NullableSet = Set<NodeId>

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
           let i, branches = rule

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
           let i, branches = rule

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

type private FirstSet = Map<TermId, Set<TokenId>>

let private computeFirstOf (nullableSet: NullableSet) (firstSet: FirstSet) (terms: Term list) =
  let rec go acc terms =
    match terms with
    | [] -> acc

    | term :: terms ->
      let i = Term.id term

      let acc = firstSet |> getMultiset i |> Set.union acc

      if Set.contains i nullableSet then
        go acc terms
      else
        acc

  go Set.empty terms

let private computeFirstSet (termArray: Term array) (nullableSet: NullableSet) (rules: Rule list) : FirstSet =
  let firstOf (acc: FirstSet) terms = computeFirstOf nullableSet acc terms

  let firstSet: FirstSet =
    termArray
    |> Array.fold
         (fun acc term ->
           match term with
           | Term.Token (ti, _) -> Map.add ti (Set.singleton ti) acc
           | Term.Node _ -> acc)
         Map.empty

  let update firstSet =
    rules
    |> List.fold
         (fun (modified, firstSet) rule ->
           let i, branches = rule

           let prev = firstSet |> getMultiset i

           let next =
             branches
             |> List.map (fun (_, _, _, terms) -> firstOf firstSet terms)
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

  makeClosure firstSet

// -----------------------------------------------
// Follow
// -----------------------------------------------

type private FollowSet = Map<TermId, Set<TermId>>

type private FollowAcc = bool * FollowSet

let private computeFollowSet (nullableSet: NullableSet) (firstSet: FirstSet) (rules: Rule list) : FollowSet =
  let isNullable i = nullableSet |> Set.contains i

  let add (modified, acc) (i: TermId) (follower: TermId) =
    if acc
       |> getMultiset i
       |> Set.contains follower
       |> not then
      true, addMultiset i follower acc
    else
      modified, acc

  let extend acc (i: TermId) (followers: Set<TermId>) =
    followers
    |> Set.fold (fun acc follower -> add acc i follower) acc

  let rec update followSet =
    rules
    |> List.fold
         (fun acc (rule, branches) ->
           branches
           |> List.fold
                (fun acc (_, _, _, terms) ->
                  let acc =
                    let rec go acc terms =
                      match terms with
                      | term :: ((next :: _) as terms) ->
                        let acc =
                          let next = getMultiset (Term.id next) firstSet
                          extend acc (Term.id term) next

                        go acc terms

                      | _ -> acc

                    go acc terms

                  let acc =
                    let followers =
                      let _, followSet = acc
                      getMultiset rule followSet

                    let rec go acc terms =
                      match terms with
                      | t :: terms ->
                        let acc = extend acc (Term.id t) followers

                        if isNullable (Term.id t) then
                          go acc terms
                        else
                          acc

                      | [] -> acc

                    go acc (List.rev terms)

                  acc)
                acc)
         (false, followSet)

  let rec makeClosure acc =
    let modified, acc = update acc

    if modified then
      makeClosure acc
    else
      acc

  makeClosure Map.empty

// -----------------------------------------------
// LR Parser Generation
// -----------------------------------------------

// type private NodeRef = Node

/// Production rule (`X -> A B`.)
type private BranchData = NodeId * Term list

type private BranchId = int

/// LR(1)-term
type private Lr1Term = Lr1Term of BranchId * dotPos: int * lookahead: TokenId

/// State of DFA for LR(1).
type private Lr1State = Set<Lr1Term>

/// Interned ID of state.
type private StateId = int

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private LrAction =
  | Shift of StateId
  | Jump of StateId
  | Reduce of NodeId * width: int

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type LrParser =
  private
    { Table: Map<StateId * TermId, LrAction>
      InitialState: StateId
      AcceptSet: Set<StateId>
      TermArray: Term array
      TokenMemo: TokenMemo }

let generateLrParser (grammarText: string) : LrParser =
  let termArray, tokenMemo, nodeMemo, root, rules = parseGrammar grammarText |> lower

  let nullableSet = computeNullableSet rules
  let firstSet = computeFirstSet termArray nullableSet rules
  let followSet = computeFollowSet nullableSet firstSet rules

  let stateArray = ResizeArray<Lr1State>()
  let stateMemo = Dictionary<Lr1State, StateId>()
  let edgeMap = Dictionary<StateId * TermId, StateId>()
  let generatedStateStack = System.Collections.Generic.Stack()

  let branchArray, branchPrec, ruleBranches =
    let acc = ResizeArray()
    let branchPrec = ResizeArray()
    let ruleBranches = Array.replicate termArray.Length Array.empty

    rules
    |> List.fold
         (fun bi (ruleId, branches) ->
           let branchIds = ResizeArray()

           let bi =
             branches
             |> List.fold
                  (fun bi (b: Branch) ->
                    let _, name, prec, terms = b

                    eprintfn
                      "branch %d:%s %s -> %s"
                      bi
                      name
                      (Term.toString termArray.[ruleId])
                      (terms
                       |> List.map Term.toString
                       |> String.concat "")

                    acc.Add(ruleId, terms |> List.toArray)
                    branchPrec.Add(prec)
                    branchIds.Add(bi)
                    bi + 1)
                  bi

           eprintfn
             "rule-branches %d:%s -> %s"
             ruleId
             (Term.toString termArray.[ruleId])
             (branchIds |> Seq.map string |> String.concat ", ")

           ruleBranches.[ruleId] <- branchIds.ToArray()
           bi)
         0
    |> ignore

    acc.ToArray(), branchPrec.ToArray(), ruleBranches

  let computeFirstOf (terms: Term array) : Set<TokenId> =
    let rec go state i =
      if i < terms.Length then
        let state =
          match terms.[i] with
          | Term.Node (nodeId, _) -> Set.union state (getMultiset nodeId firstSet)
          | Term.Token (tokenId, _) -> Set.add tokenId state

        let nullable =
          match terms.[i] with
          | Term.Node (nodeId, _) -> Set.contains nodeId nullableSet
          | Term.Token _ -> false

        if nullable then
          go state (i + 1)
        else
          state
      else
        state

    go Set.empty 0

  let internState (s: Lr1State) =
    match stateMemo.TryGetValue(s) with
    | true, it -> it
    | false, _ ->
      let n = stateArray.Count
      stateArray.Add(s)
      stateMemo.Add(s, n)

      eprintfn
        "state %d:[%s\n]"
        n
        (s
         |> Set.toList
         |> List.map (fun (lt: Lr1Term) ->
           let (Lr1Term (bi, dot, lookahead)) = lt
           let la = Term.toString termArray.[lookahead]
           let node, terms = branchArray.[bi]

           let rhs =
             if dot < terms.Length then
               terms
               |> Array.mapi (fun i term ->
                 if i = dot then
                   "*" + Term.toString term
                 else
                   Term.toString term)
               |> String.concat " "
             else
               (terms
                |> Array.map Term.toString
                |> String.concat " ")
               + " *"

           sprintf "\n  %s -> %s, %s" (Term.toString termArray.[node]) rhs la)
         |> String.concat "")

      generatedStateStack.Push(n)
      n

  // LR(1)項の集合の閉包をとることで、それらのLR(1)項を含むDFAの状態を生成する
  let getClosure (seed: Lr1State) : StateId =
    let addLt (modified, state) (lt: Lr1Term) =
      if state |> Set.contains lt |> not then
        true, Set.add lt state
      else
        modified, state

    let rec go acc =
      let _, state = acc

      state
      |> Set.fold
           (fun acc (lt: Lr1Term) ->
             let (Lr1Term (branchId, dot, lookahead)) = lt
             let _, terms = branchArray.[branchId]

             if dot < terms.Length then
               match terms.[dot] with
               | Term.Node (nodeId, _) ->
                 let lookaheadSet =
                   let rest = Array.append terms.[dot + 1 ..] [| termArray.[lookahead] |]
                   computeFirstOf rest

                 ruleBranches.[nodeId]
                 |> Array.fold
                      (fun acc bi ->
                        lookaheadSet
                        |> Set.fold
                             (fun acc lookahead ->
                               let lt = Lr1Term(bi, 0, lookahead)
                               addLt acc lt)
                             acc)
                      acc

               | Term.Token _ -> acc
             else
               acc)
           acc

    let rec makeClosure state =
      let modified, state = go (false, state)

      if modified then
        makeClosure state
      else
        state

    internState (makeClosure seed)

  // 状態から出る辺と、遷移先の状態を生成する
  let genAdjacentEdges (s: StateId) =
    stateArray.[s]
    |> Set.toList
    |> List.choose (fun (lt: Lr1Term) ->
      // LR(1)項のドットを1つ右に進めて、その際に飛び越える項と、更新後のLR(1)項を得る
      // ドットが右端だったらNone
      let (Lr1Term (branchId, dot, lookahead)) = lt
      let _, terms = branchArray.[branchId]

      if dot + 1 <= terms.Length then
        Some(Term.id terms.[dot], Lr1Term(branchId, dot + 1, lookahead))
      else
        None)
    |> Seq.groupBy fst
    |> Seq.map (fun (termId, group) ->
      let nextState =
        let ltList = group |> Seq.map snd |> Set.ofSeq
        getClosure ltList

      eprintfn "edge s#%d, %d:%s -> s#%d" s termId (Term.toString termArray.[termId]) nextState
      // check conflict; compare precedence of lt
      edgeMap.[(s, termId)] <- nextState)
    |> Seq.iter ignore

  // Generate DFA.
  let initialState =
    ruleBranches.[root]
    |> Array.map (fun bi -> Lr1Term(bi, 0, Eof))
    |> Set.ofArray
    |> getClosure

  while generatedStateStack.Count <> 0 do
    let state = generatedStateStack.Pop()
    genAdjacentEdges state

  // Convert DFA to table.
  let table = Dictionary<StateId * TermId, LrAction>()
  let mutable acceptSet: Set<StateId> = Set.empty

  for (KeyValue ((stateId, termId), nextStateId)) in edgeMap do
    table.[(stateId, termId)] <-
      match termArray.[termId] with
      | Term.Token _ -> LrAction.Shift nextStateId
      | Term.Node _ -> LrAction.Jump nextStateId

  for stateId, state in stateArray |> Seq.indexed do
    for lt in state do
      let (Lr1Term (branchId, dot, lookahead)) = lt
      let node, terms = branchArray.[branchId]

      if dot = terms.Length then
        if node = root then
          eprintfn "accept %d" stateId
          acceptSet <- Set.add stateId acceptSet

        eprintfn
          "reduce %d, %d:%s to %d:%s (%d)"
          stateId
          lookahead
          (Term.toString termArray.[lookahead])
          node
          (Term.toString termArray.[node])
          terms.Length

        // check conflict; compare precedence of lt
        table.[(stateId, lookahead)] <- LrAction.Reduce(node, terms.Length)

  { Table =
      table
      |> Seq.map (fun (KeyValue (key, value)) -> key, value)
      |> Map.ofSeq

    InitialState = initialState
    AcceptSet = acceptSet
    TermArray = termArray
    TokenMemo = tokenMemo }

module LrParser =
  let parse (tokens: string list) (p: LrParser) =
    let tokenMemo = Array.append (Array.ofList tokens) [| "$"; "$$" |]

    let tokens =
      let tokens =
        tokens
        |> List.map (fun token ->
          p.TokenMemo
          |> Map.tryFind token
          |> Option.defaultWith (fun () -> failwithf "Unknown token '%s'" token))

      List.append tokens [ Eof; Eof ]

    let mutable cursor = 0

    let rec go state stack tokens =
      match tokens with
      | token :: tokenTail ->
        match p.Table |> Map.tryFind (state, token) with
        | Some (LrAction.Shift next) ->
          eprintfn "shift %d:%s at %d" token (tokenMemo.[cursor]) cursor
          cursor <- cursor + 1
          go next (state :: stack) tokenTail

        | Some (LrAction.Reduce (nodeId, width)) ->
          let items, stack = List.splitAt width (state :: stack)
          eprintfn "reduce %A -> %d" items nodeId

          let state =
            match stack with
            | state :: _ -> state
            | _ -> failwith "empty stack"

          match p.Table |> Map.tryFind (state, nodeId) with
          | Some (LrAction.Jump next) -> go next stack tokens
          | _ -> failwith "invalid table: jump expected"

        | _ ->
          if Set.contains state p.AcceptSet then
            eprintfn "accept"
          else
            failwithf "parse failed at %d:%A" cursor tokenMemo.[cursor]

      | _ -> failwith "unreachable"

    go p.InitialState [] tokens

// -----------------------------------------------
// Dump
// -----------------------------------------------

let dump text =
  let termArray, tokenMemo, nodeMemo, startRule, rules = parseGrammar text |> lower

  let nullableSet = computeNullableSet rules
  let firstSet = computeFirstSet termArray nullableSet rules
  let followSet = computeFollowSet nullableSet firstSet rules

  let textOf termId = Term.toString termArray.[termId]

  printfn
    "nullable %s"
    (rules
     |> List.choose (fun (nodeId, _) ->
       if nullableSet |> Set.contains nodeId then
         Some(textOf nodeId)
       else
         None)
     |> String.concat ", ")

  printfn "first:"

  for nodeId, _ in rules do
    let tokens =
      firstSet
      |> getMultiset nodeId
      |> Set.toList
      |> List.map (fun tokenId -> Term.toString termArray.[tokenId])
      |> String.concat ", "

    printfn "  %s: %s" (textOf nodeId) tokens

  printfn "follow:"

  for i in 0 .. termArray.Length - 1 do
    let tokens =
      followSet
      |> getMultiset i
      |> Set.toList
      |> List.map textOf
      |> String.concat ", "

    printfn "  %s: %s" (textOf i) tokens
