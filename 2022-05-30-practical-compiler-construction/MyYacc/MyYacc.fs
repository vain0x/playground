module MyYacc

module MyLex = MyLex

type private Dictionary<'K, 'T> = System.Collections.Generic.Dictionary<'K, 'T>

let inline private unreachable () = failwith "unreachable"

/// Gets a set of items from a multiset.
let private getMultiset key map =
  Map.tryFind key map
  |> Option.defaultValue Set.empty

/// Adds an item to a multiset.
let private addMultiset key value map =
  Map.add key (Set.add value (getMultiset key map)) map

// -----------------------------------------------
// Trace
// -----------------------------------------------

// Set true to print trace logs.
[<Literal>]
let private Trace = false

let private trace fmt =
  Printf.kprintf (if Trace then eprintf "%s\n" else ignore) fmt

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

/// (生成規則の右辺の)項
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private Term =
  | Token of TermId * name: TokenText
  | Node of TermId * name: TokenText

type private TermMap = Map<TermId, Term>
type private TokenMemo = Map<TokenText, TokenId>

/// branchId, name, precedence, terms
type private Branch = string * (int * Assoc) * Term list
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

/// 終端トークンのID (`$`)
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
               trace "node %d:%s" i t
               Term.Node(i, t), tMemo, Map.add t i nMemo
             else
               trace "token %d:%s" i t
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
    |> List.map (fun (start, (ruleName, branches)) ->
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
              |> Option.defaultValue (bi, Assoc.NonAssoc)

          let terms =
            b.Terms
            |> List.map (fun t ->
              match t with
              | ParsedTerm.Token t -> Term.Token(internToken t, t)
              | ParsedTerm.Node t ->
                match ruleMemo |> Map.tryFind t with
                | Some i -> Term.Node(i, t)
                | None -> failwithf "Undefined node '%s'" name)

          let terms =
            if start then
              List.append terms [ Term.Token(Eof, "$") ]
            else
              terms

          name, prec, terms)

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
      let _, _, terms = b
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
             |> List.exists (fun (_, _, terms) ->
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
             |> List.map (fun (_, _, terms) -> firstOf firstSet terms)
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

// LRパースでFOLLOW集合は使わない。SLRでは使う

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
                (fun acc (_, _, terms) ->
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

/// Production rule (`X -> A B`.)
type private BranchData = NodeId * Term list

type private BranchId = int

/// LR(1)-term
///
/// skip: the number of nullable nodes skipped
type private Lr1Term = Lr1Term of BranchId * dotPos: int * skip: int * lookahead: TokenId

/// State of DFA for LR(1).
type private Lr1State = Set<Lr1Term>

/// Interned ID of state.
type private StateId = int

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private LrAction =
  | Shift of StateId
  /// 還元後の遷移
  | Jump of StateId
  /// `width`: ポップする状態の個数 (生成規則の右辺にある項の個数)
  | Reduce of NodeId * BranchId * width: int
  | Accept of BranchId

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type LrParser =
  private
    { Table: Map<StateId * TermId, LrAction>
      InitialState: StateId
      AcceptSet: Set<StateId>
      TermArray: Term array
      TokenMemo: TokenMemo
      // branchId -> name of reduced node
      BranchArray: string array }

let generateLrParser (grammarText: string) : LrParser =
  let termArray, tokenMemo, _nodeMemo, root, rules = parseGrammar grammarText |> lower

  let nullableSet = computeNullableSet rules
  let firstSet = computeFirstSet termArray nullableSet rules
  // let followSet = computeFollowSet nullableSet firstSet rules

  let stateArray = ResizeArray<Lr1State>()
  let stateMemo = Dictionary<Lr1State, StateId>()
  let edgeMap = Dictionary<StateId * TermId, StateId>()
  let generatedStateStack = System.Collections.Generic.Stack()
  let branchNames = ResizeArray()

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
                    let name, prec, terms = b

                    trace
                      "branch %d:%s %s -> %s"
                      bi
                      name
                      (Term.toString termArray.[ruleId])
                      (terms
                       |> List.map Term.toString
                       |> String.concat " ")

                    acc.Add(ruleId, terms |> List.toArray)
                    branchPrec.Add(prec)
                    branchIds.Add(bi)
                    branchNames.Add(name)
                    bi + 1)
                  bi

           trace
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

      trace
        "state s#%d:[%s\n]"
        n
        (s
         |> Set.toList
         |> List.map (fun (lt: Lr1Term) ->
           let (Lr1Term (bi, dot, _, lookahead)) = lt
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
             let (Lr1Term (branchId, dot, skip, lookahead)) = lt
             let _, terms = branchArray.[branchId]

             if dot < terms.Length then
               match terms.[dot] with
               | Term.Node (nodeId, _) ->
                 let lookaheadSet =
                   let rest = Array.append terms.[dot + 1 ..] [| termArray.[lookahead] |]
                   computeFirstOf rest

                 let acc =
                   ruleBranches.[nodeId]
                   |> Array.fold
                        (fun acc bi ->
                          lookaheadSet
                          |> Set.fold
                               (fun acc lookahead ->
                                 let lt = Lr1Term(bi, 0, 0, lookahead)
                                 addLt acc lt)
                               acc)
                        acc

                 // 空許容なノードを飛び越えた後の項を加える
                 let acc =
                   if nullableSet |> Set.contains nodeId then
                     let lt = Lr1Term(branchId, dot + 1, skip + 1, lookahead)
                     addLt acc lt
                   else
                     acc

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
      let (Lr1Term (branchId, dot, skip, lookahead)) = lt
      let _, terms = branchArray.[branchId]

      if dot + 1 <= terms.Length then
        Some(Term.id terms.[dot], Lr1Term(branchId, dot + 1, skip, lookahead))
      else
        None)
    |> Seq.groupBy fst
    |> Seq.map (fun (termId, group) ->
      let nextState =
        let ltList = group |> Seq.map snd |> Set.ofSeq
        getClosure ltList

      trace "edge s#%d, %d:%s -> s#%d" s termId (Term.toString termArray.[termId]) nextState
      edgeMap.[(s, termId)] <- nextState)
    |> Seq.iter ignore

  // Generate DFA.
  let initialState =
    ruleBranches.[root]
    |> Array.map (fun bi -> Lr1Term(bi, 0, 0, Eof))
    |> Set.ofArray
    |> getClosure

  while generatedStateStack.Count <> 0 do
    let state = generatedStateStack.Pop()
    genAdjacentEdges state

  // Convert DFA to table.
  let table = Dictionary<StateId * TermId, LrAction>()
  let tablePrec = Dictionary<StateId * TermId, int>()
  let mutable acceptSet: Set<StateId> = Set.empty

  let computeEdgePrec stateId termId =
    stateArray.[stateId]
    |> Set.fold
         (fun p (lt: Lr1Term) ->
           let (Lr1Term (branchId, dot, _, _)) = lt
           let _, terms = branchArray.[branchId]

           if dot < terms.Length && Term.id terms.[dot] = termId then
             let q, _ = branchPrec.[branchId]
             max p q
           else
             p)
         0

  let precAt stateId termId =
    match tablePrec.TryGetValue((stateId, termId)) with
    | true, it -> it
    | false, _ -> 0

  for (KeyValue ((stateId, termId), nextStateId)) in edgeMap do
    table.[(stateId, termId)] <-
      match termArray.[termId] with
      | Term.Token _ -> LrAction.Shift nextStateId
      | Term.Node _ -> LrAction.Jump nextStateId

    tablePrec.[(stateId, termId)] <- computeEdgePrec stateId termId

  for stateId, state in stateArray |> Seq.indexed do
    for lt in state do
      let (Lr1Term (branchId, dot, skip, lookahead)) = lt
      let node, terms = branchArray.[branchId]
      let prec, _ = branchPrec.[branchId]

      let reducible =
        terms[dot..]
        |> Array.forall (fun term -> Set.contains (Term.id term) nullableSet)

      if reducible then
        trace
          "reduce %d, %d:%s to %d:%s (%d)"
          stateId
          lookahead
          (Term.toString termArray.[lookahead])
          node
          (Term.toString termArray.[node])
          (terms.Length - skip)

        if prec < precAt stateId lookahead then
          trace "  less priority"
        else if terms.Length = 0 then
          trace "  empty branch"
        else
          // スタックからポップする状態の個数
          // スキップした空許容なノードに対応する状態がスタックに配置されてないので、スキップした数だけポップする数を減らす
          let width = dot - skip

          table.[(stateId, lookahead)] <- LrAction.Reduce(node, branchId, width)
          tablePrec.[(stateId, lookahead)] <- prec

          if node = root then
            trace "accept %d" stateId
            acceptSet <- Set.add stateId acceptSet
            table.[(stateId, Eof)] <- LrAction.Accept branchId

  { Table =
      table
      |> Seq.map (fun (KeyValue (key, value)) -> key, value)
      |> Map.ofSeq

    InitialState = initialState
    AcceptSet = acceptSet
    TermArray = termArray
    TokenMemo = tokenMemo
    BranchArray = branchNames.ToArray() }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ParseEvent =
  | Token of index: int
  | StartNode of name: string * count: int

module LrParser =
  let parse (tokens: string list) (p: LrParser) =
    let tokenMemo = Array.append (Array.ofList tokens) [| "$"; "$$" |]
    let tokenCount = tokenMemo.Length - 2

    let builder = ResizeArray<ParseEvent list>()

    let onShift (i: int) =
      if i < tokenCount then
        builder.Add([ ParseEvent.Token i ])

    let onReduce (name: string) (count: int) =
      let index = builder.Count - count

      let children =
        builder.GetRange(index, count)
        |> Seq.collect id
        |> Seq.toList

      builder.RemoveRange(index, count)
      builder.Add(ParseEvent.StartNode(name, count) :: children)

    let onAccept name =
      for b in builder do
        trace "%A" b

      let count = builder.Count
      let children = builder |> Seq.collect id |> Seq.toList
      ParseEvent.StartNode(name, count) :: children

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
          trace "shift %d:%s at %d => s#%d" token (tokenMemo.[cursor]) cursor next
          onShift cursor
          cursor <- cursor + 1
          go next (state :: stack) tokenTail

        | Some (LrAction.Reduce (nodeId, branchId, width)) ->
          let items, stack = List.splitAt width (state :: stack)
          trace "reduce %s(N%d -> %A) => s#%d" p.BranchArray.[branchId] nodeId items (List.head stack)
          onReduce p.BranchArray.[branchId] width

          let state =
            match stack with
            | state :: _ -> state
            | _ -> failwith "empty stack"

          match p.Table |> Map.tryFind (state, nodeId) with
          | Some (LrAction.Jump next) ->
            trace "jump => s#%d" next
            go next stack tokens

          | it -> failwithf "invalid table: jump expected %A" it

        | Some (LrAction.Accept branchId) ->
          trace "accept!"
          onAccept p.BranchArray.[branchId]

        | _ -> failwithf "parse failed at %d:%A" cursor tokenMemo.[cursor]

      | _ -> unreachable ()

    go p.InitialState [] tokens

// -----------------------------------------------
// ParseEvent
// -----------------------------------------------

module ParseEvent =
  let dump (tokens: (string * int) array) (events: ParseEvent list) =
    let rec go indent count events =
      match events with
      | _ when count = 0 -> events

      | ParseEvent.Token i :: events ->
        printfn "%s%s %s" indent "token" (fst tokens.[i])
        go indent (count - 1) events

      | ParseEvent.StartNode (name, childrenCount) :: events ->
        printfn "%s%s %s (%d)" indent "node" name childrenCount
        let events = go (indent + "  ") childrenCount events
        go indent (count - 1) events

      | [] -> unreachable ()

    let events = go "" 1 events
    printfn "rest: %A" events

// -----------------------------------------------
// Parse Tree
// -----------------------------------------------

/// Element of parse tree.
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type PElement =
  | Token of name: string * text: string
  | Node of name: string * children: PElement list

module ParseTree =
  let generate (tokens: (string * string) array) (events: ParseEvent list) : PElement =
    let rec go acc count events =
      match events with
      | _ when count = 0 -> List.rev acc, events

      | ParseEvent.Token i :: events ->
        let kind, text = tokens.[i]
        go (PElement.Token(kind, text) :: acc) (count - 1) events

      | ParseEvent.StartNode (name, childrenCount) :: events ->
        let children, events = go [] childrenCount events
        go (PElement.Node(name, children) :: acc) (count - 1) events

      | [] -> unreachable ()

    let children, events = go [] 1 events

    if List.isEmpty events |> not then
      eprintfn "rest = %A" events
      unreachable ()

    children
    |> List.tryExactlyOne
    |> Option.defaultWith unreachable

// -----------------------------------------------
// Interface
// -----------------------------------------------

let generateParser grammarText =
  try
    generateLrParser grammarText
  with
  | ParseGrammarException (msg, i) ->
    eprintfn "FATAL: Invalid grammar. %s at %d" msg i
    exit 1

let parseTokensToTree tokens (parser: LrParser) =
  let events =
    let tokens = tokens |> Array.map fst |> Array.toList
    LrParser.parse tokens parser

  ParseTree.generate tokens events

// -----------------------------------------------
// Dump
// -----------------------------------------------

let dump text =
  let termArray, _tokenMemo, _nodeMemo, _startRule, rules = parseGrammar text |> lower

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
