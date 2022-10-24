module ParserV2

open System

type private Queue<'T> = System.Collections.Generic.Queue<'T>
type private Stack<'T> = System.Collections.Generic.Stack<'T>
type private HashSet<'T> = System.Collections.Generic.HashSet<'T>
type private HashMap<'K, 'T> = System.Collections.Generic.Dictionary<'K, 'T>

let inline private todo () = failwith "todo"
let inline private unreachable () = failwith "unreachable"
let inline private (|Unreachable|) _ = unreachable ()

let inline private swap<'T> (x: 'T byref) (y: 'T byref) =
  let t = x
  x <- y
  y <- t

module private HashMap =
  let inline internal findOr key alt (map: HashMap<_, _>) =
    match map.TryGetValue(key) with
    | true, it -> it
    | _ -> alt

  let inline internal tryFind key (map: HashMap<_, _>) =
    match map.TryGetValue(key) with
    | true, value -> Some value
    | _ -> None

module private ParserCombinator =
  /// Kind of token.
  type private K = int

  type private IParseContext =
    abstract Shift: unit -> obj
    abstract Push: obj -> unit
    abstract Pop: unit -> obj

  type private SemanticAction = IParseContext -> unit

  /// Term of syntax rule.
  [<RequireQualifiedAccess; ReferenceEquality; NoComparison>]
  type private Term =
    private
    | Expect of K * cut: bool * SemanticAction
    | Symbol of Symbol
    | Eps of SemanticAction
    | Map of Term * SemanticAction
    | Seq of Term list * SemanticAction
    | Choice of Term list

    /// `leftRec(L, R) = mu X. (L | X R)`
    | LeftRec of left: Term * right: Term * folder: SemanticAction

    | Fix of local: Symbol * body: Term

  and [<ReferenceEqualityAttribute; NoComparisonAttribute; StructuredFormatDisplay("&{name}")>] private Symbol =
    private | Symbol of id: obj * name: string * Lazy<Term>

  [<Struct; NoEquality; NoComparison>]
  type Rule<'T, 'N> = private Rule of Term

  [<NoEquality; NoComparison>]
  type RecRule<'T, 'N> = private RecRule of Symbol * termRef: Term option ref

  [<Struct; NoEquality; NoComparison>]
  type Binding = private Binding of id: obj * name: string * Term

  [<NoEquality; NoComparison>]
  type GrammarData =
    private
      { Start: Symbol
        RuleArray: Symbol array
        RuleMemo: HashMap<obj, int> }

  type Grammar<'T, 'N> = Grammar of GrammarData

  // tokens:

  // let look (token: int) : Rule<'T, unit> = todo ()

  /// 特定の種類のトークンが1つ出現することを表す規則
  let expect (token: int) (extractor: 'T -> 'N) : Rule<'T, 'N> =
    Rule(Term.Expect(token, false, (fun ctx -> ctx.Push(extractor (ctx.Shift() :?> 'T) :> obj))))

  /// 特定の種類のトークンが1つ出現することを表す規則
  /// また、カットを行う
  ///
  /// - カットとは、`choice` オペレータによる構文規則の選択を確定させるということ
  ///   すなわち `choice` は先読みによりこのトークンが出現するか確認して、出現していたらこの `cut` を含む規則を選択する
  let cut (token: int) (extractor: 'T -> 'N) : Rule<'T, 'N> =
    Rule(Term.Expect(token, true, (fun ctx -> ctx.Push(extractor (ctx.Shift() :?> 'T) :> obj))))

  // nominal rules:

  /// 再帰的な規則を作る
  ///
  /// - `recurse` により、定義される規則を他の規則の一部として利用できる
  /// - `build` する際に、再帰的な規則にその定義となる規則を `bind` したものを渡すこと
  ///     そうでなければエラーが発生する ("Rule xxx not bound")
  let recursive (name: string) : RecRule<'T, _> =
    let termRef: Term option ref = ref None

    let symbol =
      Symbol(
        termRef :> obj,
        name,
        lazy
          (match termRef.contents with
           | Some it -> it
           | None -> failwithf "Rule '%s' unbound" name)
      )

    RecRule(symbol, termRef)

  /// 再帰的な規則の利用を表す規則
  let recurse (rule: RecRule<'T, 'N>) : Rule<'T, 'N> =
    let (RecRule (symbol, _)) = rule
    Rule(Term.Symbol symbol)

  /// 再帰的な規則に定義を束縛することを表す
  let bind (recRule: RecRule<'T, 'N>) (actualRule: Rule<'T, 'N>) : Binding =
    let (RecRule (symbol, termRef)) = recRule
    let (Symbol (id, name, _)) = symbol
    let (Rule actualTerm) = actualRule

    assert (termRef.contents |> Option.isNone)
    termRef.contents <- Some actualTerm

    Binding(id, name, actualTerm)

  let fix (name: string) (creator: Rule<'T, 'N> -> Rule<'T, 'N>) : Rule<'T, 'N> =
    let recRule = recursive name
    let (Rule term) = creator (recurse recRule)

    let (RecRule (symbol, termRef)) = recRule
    termRef.contents <- Some term

    Rule(Term.Fix(symbol, term))

  // let label (name: string) (rule: Rule<'T, 'N>) : Rule<'T, 'N> = todo ()

  // combinators:

  // let eps () : Rule<'T, 'N> = todo ()

  /// 単一の規則とその変換
  let rule1 (r: Rule<'T, 'A>) (mapping: 'A -> 'N) : Rule<'T, 'N> =
    let (Rule r) = r

    Term.Map(
      r,
      (fun ctx ->
        let n = ctx.Pop() :?> 'A
        ctx.Push(mapping n :> obj))
    )
    |> Rule

  /// 2つの規則の並び
  ///
  /// - `decode`: 規則のパース結果を結合する関数
  let rule2 (r1: Rule<'T, 'A>) (r2: Rule<'T, 'B>) (decode: 'A -> 'B -> 'N) : Rule<'T, 'N> =
    let (Rule r1) = r1
    let (Rule r2) = r2

    Term.Seq(
      [ r1; r2 ],
      fun ctx ->
        let n2 = ctx.Pop() :?> 'B
        let n1 = ctx.Pop() :?> 'A
        ctx.Push(decode n1 n2 :> obj)
    )
    |> Rule

  /// 3つの規則の並び
  let rule3 (r1: Rule<'T, 'A>) (r2: Rule<'T, 'B>) (r3: Rule<'T, 'C>) (decode: 'A -> 'B -> 'C -> 'N) : Rule<'T, 'N> =
    let (Rule r1) = r1
    let (Rule r2) = r2
    let (Rule r3) = r3

    Term.Seq(
      [ r1; r2; r3 ],
      fun ctx ->
        let n3 = ctx.Pop() :?> 'C
        let n2 = ctx.Pop() :?> 'B
        let n1 = ctx.Pop() :?> 'A
        ctx.Push(decode n1 n2 n3 :> obj)
    )
    |> Rule

  /// 4つの規則の並び
  let rule4
    (r1: Rule<'T, 'A>)
    (r2: Rule<'T, 'B>)
    (r3: Rule<'T, 'C>)
    (r4: Rule<'T, 'D>)
    (decode: 'A -> 'B -> 'C -> 'D -> 'N)
    : Rule<'T, 'N> =
    let (Rule r1) = r1
    let (Rule r2) = r2
    let (Rule r3) = r3
    let (Rule r4) = r4

    Term.Seq(
      [ r1; r2; r3; r4 ],
      fun ctx ->
        let n4 = ctx.Pop() :?> 'D
        let n3 = ctx.Pop() :?> 'C
        let n2 = ctx.Pop() :?> 'B
        let n1 = ctx.Pop() :?> 'A
        ctx.Push(decode n1 n2 n3 n4 :> obj)
    )
    |> Rule

  let rule5 r1 r2 r3 r4 r5 decode =
    rule4 r1 r2 r3 (rule2 r4 r5 (fun n4 n5 -> n4, n5)) (fun n1 n2 n3 (n4, n5) -> decode n1 n2 n3 n4 n5)

  let rule6 r1 r2 r3 r4 r5 r6 decode =
    rule4 r1 r2 r3 (rule3 r4 r5 r6 (fun n4 n5 n6 -> n4, n5, n6)) (fun n1 n2 n3 (n4, n5, n6) -> decode n1 n2 n3 n4 n5 n6)

  let rule7 r1 r2 r3 r4 r5 r6 r7 decode =
    rule4 r1 r2 r3 (rule4 r4 r5 r6 r7 (fun n4 n5 n6 n7 -> n4, n5, n6, n7)) (fun n1 n2 n3 (n4, n5, n6, n7) ->
      decode n1 n2 n3 n4 n5 n6 n7)

  let eps value =
    Rule(Term.Eps(fun ctx -> ctx.Push(value)))

  /// 複数の規則のうち、適用可能なものを1つ選択する規則
  ///
  /// - `rules` は適用可能な規則の候補からなるリストである
  ///     - それぞれの候補はカットを含む規則でなければいけない
  let choice (rules: Rule<'T, 'N> list) : Rule<'T, 'N> =
    Rule(Term.Choice(List.map (fun (Rule r) -> r) rules))

  let opt (rule: Rule<'T, 'A>) (decode: ('A option -> 'N)) : Rule<'T, 'N> =
    choice [ rule1 rule (fun a -> decode (Some a))
             eps None ]

  let leftRec (pLeft: Rule<'T, 'N>) (pRight: Rule<'T, 'A>) (decode: 'N -> 'A -> 'N) : Rule<'T, 'N> =
    let (Rule left) = pLeft
    let (Rule right) = pRight

    Rule(
      Term.LeftRec(
        left,
        right,
        (fun ctx ->
          let right = ctx.Pop() :?> 'A
          let left = ctx.Pop() :?> 'N
          ctx.Push(decode left right :> obj))
      )
    )

  /// 左結合な二項演算の規則
  ///
  /// - 典型例としては乗算 (`E * E`) の式をパースするのに使う
  ///     - 仮に乗算の両辺の規則を `primary` とする
  ///     - 乗算の規則は `infixLeft primary (P.rule2 (P.expect '*') primary (fun _ e -> e)) (fun l r -> Mul(l, r))` のように書ける
  /// - `infixLeft l r` はBNF風の記法で書けば以下の構文を表す
  ///     - `A = l | X r`
  ///     - いわゆる「左再帰」の構文である。`choice` と `recurse` の組み合わせで書いてしまうと、パース処理が無限再帰に陥る
  let infixLeft left mid right decode =
    leftRec left (rule2 mid right (fun m r -> m, r)) (fun l (m, r) -> decode l m r)

  // 0+ repetition
  let rep (pItem: Rule<'T, 'N>) : Rule<'T, 'N list> =
    let (Rule item) = pItem
    let (Rule e) = eps (([]: 'N list) :> obj)

    let t =
      Term.LeftRec(
        e,
        item,
        (fun ctx ->
          let right = ctx.Pop() :?> 'N
          let left = ctx.Pop() :?> 'N list
          ctx.Push((right :: left) :> obj))
      )

    rule1 (Rule t) List.rev

  // ---------------------------------------------
  // Left-up
  // ---------------------------------------------

  /// Terminal.
  [<RequireQualifiedAccess; NoEquality; NoComparison>]
  type private LSingle =
    | Token of K
    | Symbol of Symbol

  /// Lefty term: Terminal and following term, or eps.
  [<RequireQualifiedAccess; NoEquality; NoComparison>]
  type private LTermData =
    | Eps
    | Single of LSingle
    // notice rhs is non-lefty.
    | Pair of LSingle * Term
    | Choice of LTerm list

  /// Lefty-term.
  and [<NoEquality; NoComparison>] private LTerm = LTerm of SemanticAction list * LTermData * SemanticAction list

  /// Converts a term to lefty form.
  let rec private leftUp term : LTerm =
    match term with
    | Term.Expect (k, _, action) -> LTerm([], LTermData.Single(LSingle.Token k), [ action ])

    | Term.Symbol symbol
    | Term.Fix (symbol, _) -> LTerm([], LTermData.Single(LSingle.Symbol symbol), [])

    | Term.Eps action -> LTerm([], LTermData.Eps, [ action ])

    | Term.Map (t, action) ->
      eprintfn "map"
      let (LTerm (preActions, t, actions)) = leftUp t
      LTerm(preActions, t, List.append actions [ action ])

    | Term.Seq (ts, action) ->
      eprintfn "seq"
      let (t :: ts | Unreachable (t, ts)) = ts

      let t = leftUp t

      let ts =
        match ts with
        | [] -> Term.Eps action
        | _ -> Term.Seq(ts, action)

      appendLt t ts

    | Term.Choice ts -> LTerm([], LTermData.Choice(ts |> List.map leftUp), [])

    | Term.LeftRec (_left, _right, action) ->
      // TODO: implement this
      // let left = leftUp left
      // appendLt left (Term.LeftRec(Term.Eps ignore, right, action))
      eprintfn "warn: skip infix"
      LTerm([], LTermData.Eps, [])

  and private appendLt t1 (t2: Term) : LTerm =
    let mergeActions actions =
      match actions with
      | [] -> ignore
      | [ a ] -> a

      | a :: actions -> fun ctx -> List.iter (fun a -> a ctx) actions

    let (LTerm (preActions1, t1, postActions1)) = t1

    match t1 with
    | LTermData.Eps ->
      let (LTerm (preActions2, t, actions2)) = leftUp t2

      let preActions2 =
        List.collect
          id
          [ preActions1
            postActions1
            preActions2 ]

      LTerm(preActions2, t, actions2)

    | LTermData.Single single ->
      let t2 =
        Term.Seq(
          [ Term.Eps(mergeActions postActions1)
            t2 ],
          ignore
        )

      LTerm(preActions1, LTermData.Pair(single, t2), [])

    | LTermData.Pair (single, t1) ->
      LTerm(
        preActions1,
        LTermData.Pair(
          single,
          Term.Seq(
            [ Term.Map(t1, mergeActions postActions1)
              t2 ],
            ignore
          )
        ),
        []
      )

    | LTermData.Choice branches ->
      // postActions1, t2 are cloned many
      let branches =
        branches
        |> List.map (fun b ->
          let (LTerm (bp, bt, bq)) = b
          appendLt (LTerm(bp, bt, List.append postActions1 bq)) t2)

      LTerm(preActions1, LTermData.Choice branches, [])

  // build:

  let private doBuild<'T> (start: Symbol) (bindings: Binding list) : GrammarData =
    // indexing
    let ruleArray = ResizeArray()
    let ruleRev = System.Collections.Generic.Dictionary()

    let dummy = Symbol(obj (), "dummy", lazy (Term.Choice []))

    ruleArray.Add(dummy)
    ruleRev.Add(dummy :> obj, 0)

    let ruleIdOf (rule: Symbol) =
      let (Symbol (id, _, _)) = rule
      id

    let indexOf (rule: Symbol) =
      let id = ruleIdOf rule

      if ruleRev.ContainsKey(id) |> not then
        failwithf "Rule not indexed (id:%A, rule:%A)" id rule

      ruleRev.[id]

    let intern (rule: Symbol) = ruleArray.[indexOf rule]

    (let internSymbol symbol =
      let (Symbol (id, _, _)) = symbol

      if ruleRev.ContainsKey(id) |> not then
        let index = ruleArray.Count
        ruleArray.Add(symbol)
        ruleRev.Add(id, index)

     let rec go (rule: Term) =
       match rule with
       | Term.Expect _
       | Term.Eps _ -> ()

       | Term.Symbol symbol -> internSymbol symbol

       | Term.Map (r, _) -> go r

       | Term.Seq (rules, _) ->
         for r in rules do
           go r

       | Term.Choice rules ->
         for r in rules do
           go r

       | Term.LeftRec (left, right, _) ->
         go left
         go right

       | Term.Fix (symbol, body) ->
         // After the pass, Fix behaves the same as Symbol
         // since the body is registered in the RuleArray (the symbol is now a nominal rule).
         internSymbol symbol
         go body

     for Binding (id, name, term) in bindings do
       internSymbol (Symbol(id, name, lazy (term)))
       go term)

    // dump
    (let rec hasChoice rule =
      match rule with
      | Term.Map (r, _) -> hasChoice r
      | Term.Seq (rules, _) -> rules |> List.exists hasChoice
      | Term.Choice _ -> true
      | _ -> false

     let rec go nl (rule: Term) =
       match rule with
       | Term.Symbol symbol ->
         let (Symbol (_, name, _)) = symbol
         sprintf "R%d:%s" (indexOf symbol) name

       | Term.Expect (token, _, _) -> sprintf "expect(%A)" token
       | Term.Eps _ -> "eps"
       | Term.Map (r, _) -> go nl r
       | Term.Seq (rules, _) -> sprintf "(%s)" (rules |> List.map (go nl) |> String.concat " ")

       | Term.Choice rules ->
         sprintf
           "(%s)"
           (rules
            |> List.map (go (nl + "  "))
            |> String.concat (nl + "| "))
       | Term.LeftRec (left, right, _) -> sprintf "leftRec(%s, %s)" (go nl left) (go nl right)

       | Term.Fix (local, body) ->
         let (Symbol (_, name, _)) = local
         sprintf "(μ%s. %s)" name (go (nl + "  ") body)

     for i in 1 .. ruleArray.Count - 1 do
       let rule = ruleArray.[i]

       let name, rule =
         let (Symbol (_, name, r)) = rule
         ":" + name, r.Value

       let initial = if hasChoice rule then "\n  " else " "
       eprintfn "R%d%s :=%s%s" i name initial (go "\n  " rule))

    for r in ruleArray do
      let (Symbol (_, _, termLazy)) = r
      eprintfn "leftUp: %A" (leftUp termLazy.Value)

    ({ Start = start
       RuleArray = ruleArray.ToArray()
       RuleMemo = ruleRev }: GrammarData)

  /// パーサを構築する
  ///
  /// - `start`: 開始規則
  /// - `bindings`: 再帰的な規則に定義をバインド(`bind`)するもの
  let build<'T, 'N> (start: Rule<'T, 'N>) (bindings: Binding list) : Grammar<'T, 'N> =
    let start, bindings =
      match start with
      | Rule (Term.Symbol _) -> start, bindings

      | _ ->
        let startRec = recursive "start"
        recurse startRec, bind startRec start :: bindings

    let start =
      match start with
      | Rule (Term.Symbol it) -> it
      | _ -> unreachable ()

    Grammar(doBuild start bindings)

  // parser methods:

  /// New parser. Deterministic by 1-token lookahead. Recursive decent.
  let parseV2<'T, 'N> (getKind: 'T -> K) (tokens: 'T array) (grammar: Grammar<'T, 'N>) : 'N =
    let (Grammar grammar) = grammar

    let mutable nullableMemo = HashMap()
    let mutable firstSetMemo = HashMap()

    // term -> (lookaheadKind -> branchTerm)
    // key must be Term.Choice
    let mutable jumpMemo = HashMap()
    // term -> branchTerm
    let mutable fallbackMemo = HashMap()

    let rec computeNullable (term: Term) =
      match term with
      | Term.Expect _ -> false
      | Term.Eps _ -> true
      | Term.Symbol (Symbol (id, _, _)) -> HashMap.findOr id false nullableMemo

      | Term.Map (t, _) -> computeNullable t
      | Term.Seq (terms, _) -> terms |> List.forall computeNullable
      | Term.Choice terms -> terms |> List.exists computeNullable

      // rhs must not be nullable
      | Term.LeftRec (left, _, _) -> computeNullable left

      | Term.Fix (local, _) ->
        let (Symbol (id, _, _)) = local
        HashMap.findOr id false nullableMemo

    // Compute nullable memo:
    (let mutable workList = Queue(grammar.RuleArray)
     let mutable nextList = Queue()
     let mutable modified = true

     while modified do
       modified <- false

       while workList.Count <> 0 do
         let (Symbol (id, name, termLazy)) as symbol = workList.Dequeue()
         assert (nullableMemo.ContainsKey(id) |> not)

         let nullable = computeNullable termLazy.Value

         eprintfn
           "compute nullable %s: %s"
           name
           (if nullable then
              "nullable"
            else
              "habited")

         if nullable then
           modified <- true
           nullableMemo.Add(id, nullable)
         else
           nextList.Enqueue(symbol)

       swap &workList &nextList
       nextList.Clear())

    let rec computeFirst (term: Term) =
      match term with
      | Term.Expect (k, _, _) -> set [ k ]
      | Term.Symbol (Symbol (id, _, _))
      | Term.Fix (Symbol (id, _, _), _) -> HashMap.findOr id Set.empty firstSetMemo
      | Term.Eps _ -> Set.empty
      | Term.Map (t, _) -> computeFirst t
      | Term.Seq (terms, _) -> computeFirstMany terms
      | Term.Choice terms -> terms |> List.map computeFirst |> Set.unionMany
      | Term.LeftRec (left, right, _) -> computeFirstMany [ left; right ]

    and computeFirstMany terms =
      match terms with
      | [] -> Set.empty
      | t :: terms ->
        if computeNullable t then
          Set.union (computeFirst t) (computeFirstMany terms)
        else
          computeFirst t

    // Compute first memo:
    (let mutable modified = true

     while modified do
       modified <- false

       for (Symbol (id, name, termLazy)) in grammar.RuleArray do
         let oldSet = HashMap.findOr id Set.empty firstSetMemo
         let firstSet = computeFirst termLazy.Value

         if firstSet <> oldSet then
           modified <- true
           eprintfn "compute first %s: %A" name (List.ofSeq firstSet)

           if firstSetMemo.ContainsKey(id) then
             firstSetMemo.[id] <- firstSet
           else
             firstSetMemo.Add(id, firstSet))

    // Compute jump memo:
    (let rec jumpRec (term: Term) =
      match term with
      | Term.Choice terms ->
        if jumpMemo.ContainsKey(term) |> not then
          for t in terms do
            jumpRec t

          let table = HashMap()

          for branchTerm in terms do
            let firstSet = computeFirst branchTerm

            if Set.isEmpty firstSet then
              if fallbackMemo.ContainsKey(term) then
                eprintfn "choice has multiple fallback paths: %A and %A" fallbackMemo.[term] branchTerm
                fallbackMemo.[term] <- branchTerm
              else
                fallbackMemo.Add(term, branchTerm)
            else
              for k in firstSet do
                if table.ContainsKey(k) then
                  eprintfn "choice ambiguous: k:%d" k
                else
                  table.Add(k, branchTerm)

          jumpMemo.Add(term, table)

      | Term.LeftRec (left, right, _) ->
        jumpRec left
        jumpRec right

      | Term.Eps _
      | Term.Expect _
      | Term.Symbol _
      | Term.Fix _ -> ()

      | Term.Map (t, _) -> jumpRec t
      | Term.Seq (terms, _) -> List.iter jumpRec terms

     for (Symbol (_, _, termLazy)) in grammar.RuleArray do
       jumpRec termLazy.Value)

    // runtime:

    let mutable index = 0
    let stack = Stack<obj>()

    let inline tokenAt i = tokens.[i]

    let shift cutting =
      assert (index < tokens.Length)
      let t = tokens.[index]
      eprintfn "shift %d:%A%s %A" index t (if cutting then "!" else "") t
      index <- index + 1
      t

    let fail msg =
      let pos =
        if index < tokens.Length then
          sprintf "%d:%A" index (tokenAt index)
        else
          "EOF"

      failwithf "Parse Error: At %s, %s" pos msg

    let onBeginNode () = stack.Count
    let onEndNode (previous: int) = assert (stack.Count = previous + 1)

    let ctx =
      { new IParseContext with
          override _.Shift() = shift false
          override _.Push(item) = stack.Push(item)
          override _.Pop() : obj = stack.Pop() }

    let rec parseRec rule =
      match rule with
      | Term.Expect (token, _, action) ->
        if index < tokens.Length then
          let t = tokenAt index

          if getKind t = token then
            action ctx
          else
            fail (sprintf "expect token '%A'" token)
        else
          fail (sprintf "expect token '%A'" token)

      | Term.Eps action -> action ctx

      | Term.Symbol symbol
      | Term.Fix (symbol, _) ->
        let (Symbol (_, name, termLazy)) = symbol

        try
          parseRec termLazy.Value
        with
        | _ ->
          eprintfn "(While parsing %s at %d)" name index
          reraise ()

      | Term.Map (r, action) ->
        parseRec r
        action ctx

      | Term.Seq (rules, action) ->
        let n = onBeginNode ()

        for r in rules do
          parseRec r

        action ctx
        onEndNode n

      | Term.Choice rules ->
        let table =
          match HashMap.tryFind rule jumpMemo with
          | Some it -> it
          | None -> failwithf "unreachable: jumpMemo missing: %A" rule

        let onFallback () =
          let fallbackTerm =
            match HashMap.tryFind rule fallbackMemo with
            | Some it -> it
            | None -> failwithf "choice doesn't have fallback choice:%A" rule

          parseRec fallbackTerm

        if index < tokens.Length then
          let token = tokenAt index
          let k = getKind token

          match HashMap.tryFind k table with
          | Some branchTerm ->
            let index =
              List.tryFindIndex ((=) branchTerm) rules
              |> Option.defaultValue (-1)

            eprintfn "choice k:%d index:%d" k index
            parseRec branchTerm

          | None ->
            eprintfn "trace: choice falling back on k:%d" k
            onFallback ()
        else
          onFallback ()

      | Term.LeftRec (left, right, action) ->
        let rightFirstSet = computeFirst right

        let rec infixLoop () =
          // here left value (or accumulated value) is on the stack

          if index < tokens.Length then
            // try to parse rhs
            let token = tokenAt index
            let k = getKind token

            if rightFirstSet.Contains(k) then
              parseRec right
              action ctx
              infixLoop ()

        let n = onBeginNode ()
        parseRec left
        infixLoop ()
        onEndNode n

    let r =
      let (Symbol (_, _, r)) = grammar.Start
      r.Value

    parseRec r

    if index <> tokens.Length then
      eprintfn "error: unexpected EOF at %d near %A" index tokens.[index]

    assert (stack.Count = 1)

    ctx.Pop() :?> 'N

  /// トークン列をパースする
  let parseArray (getKind: 'T -> K) (tokens: 'T array) (grammar: Grammar<'T, 'N>) : 'N = parseV2 getKind tokens grammar

/// 算術式のパーサの実装例
module private Arith =
  module P = ParserCombinator

  [<RequireQualifiedAccess; NoEquality; NoComparison>]
  type internal Token =
    | Number of int
    | Plus
    | Minus
    | Star
    | Slash
    | LeftParen
    | RightParen

  /// トークンの種類を表す適当な定数
  module private TokenKind =
    let Number = 1
    let Plus = 2
    let Minus = 3
    let Star = 4
    let Slash = 5
    let LeftParen = 6
    let RightParen = 7

    let ofToken token =
      match token with
      | Token.Number _ -> Number
      | Token.Plus -> Plus
      | Token.Minus -> Minus
      | Token.Star -> Star
      | Token.Slash -> Slash
      | Token.LeftParen -> LeftParen
      | Token.RightParen -> RightParen

  /// 二項演算子の種類
  [<RequireQualifiedAccess; NoEquality; NoComparison>]
  type internal Binary =
    | Add
    | Subtract
    | Multiply
    | Divide

  /// 数式の構文木
  [<RequireQualifiedAccess; NoEquality; NoComparison>]
  type internal Expr =
    | Number of int
    | Paren of Expr
    | BinOp of Binary * Expr * Expr

  let private pExpr: P.RecRule<_, Expr> = P.recursive "Expression"

  /// カッコ式の規則 (`'(' E ')'`)
  let private pParen =
    P.rule3 (P.cut TokenKind.LeftParen ignore) (P.recurse pExpr) (P.expect TokenKind.RightParen ignore) (fun _ e _ ->
      Expr.Paren e)

  let private pPrimary =
    P.choice [ P.cut TokenKind.Number (fun (Token.Number value | Unreachable value) -> Expr.Number value)
               pParen ]

  let private pMul =
    P.infixLeft
      pPrimary
      (P.choice [ P.cut TokenKind.Star (fun _ -> Binary.Multiply)
                  P.cut TokenKind.Slash (fun _ -> Binary.Divide) ])
      pPrimary
      (fun l op r -> Expr.BinOp(op, l, r))

  let private pAdd =
    P.infixLeft
      pMul
      (P.choice [ P.cut TokenKind.Plus (fun _ -> Binary.Add)
                  P.cut TokenKind.Minus (fun _ -> Binary.Subtract) ])
      pMul
      (fun l op r -> Expr.BinOp(op, l, r))

  let private sGrammarLazy: Lazy<P.Grammar<Token, Expr>> =
    lazy (P.build (P.recurse pExpr) [ P.bind pExpr pAdd ])

  let private tokenize (text: string) : Token array =
    text
      .Replace("(", "( ")
      .Replace(")", " )")
      .Split(" ")
    |> Array.filter (fun s -> s <> "")
    |> Array.map (fun s ->
      match s with
      | "(" -> Token.LeftParen
      | ")" -> Token.RightParen
      | "+" -> Token.Plus
      | "-" -> Token.Minus
      | "*" -> Token.Star
      | "/" -> Token.Slash
      | _ -> Token.Number(int s))

  let internal parseString (text: string) : Expr =
    let tokenArray = tokenize text
    P.parseV2 TokenKind.ofToken tokenArray sGrammarLazy.Value

  let internal tests () =
    let p s x =
      let actual =
        try
          parseString s |> sprintf "%A"
        with
        | ex -> sprintf "ERROR: %A" ex

      if actual = x then
        true
      else
        eprintfn "Assertion violated:\nactual: '%s'\nexpected: '%s'" actual x
        false

    assert (p "42" "Number 42")
    assert (p "(42)" "Paren (Number 42)")
    assert (p "1 + 2" "BinOp (Add, Number 1, Number 2)")
    assert (p "2 * 3" "BinOp (Multiply, Number 2, Number 3)")
    assert (p "1 + 2 * 3" "BinOp (Add, Number 1, BinOp (Multiply, Number 2, Number 3))")
    assert (p "(1 + 2) * 3" "BinOp (Multiply, Paren (BinOp (Add, Number 1, Number 2)), Number 3)")

    assert (p "1 + 2 - 3" "BinOp (Subtract, BinOp (Add, Number 1, Number 2), Number 3)")

    assert
      (p
        "2 * 3 / 5 * 7"
        "BinOp\n  (Multiply, BinOp (Divide, BinOp (Multiply, Number 2, Number 3), Number 5),\n   Number 7)")

/// 数列のパーサの実装例
module private NumberSequence =
  module P = ParserCombinator

  [<RequireQualifiedAccess; NoEquality; NoComparison>]
  type internal Token =
    | Number of int
    | Comma

  /// トークンの種類を表す適当な定数
  module private TokenKind =
    let Number = 1
    let Comma = 2

    let ofToken token =
      match token with
      | Token.Number _ -> Number
      | Token.Comma -> Comma

  let private pNumber =
    P.expect TokenKind.Number (fun (Token.Number value | Unreachable value) -> value)

  let private pSeq =
    P.opt
      (P.rule2
        pNumber
        // (, n)*
        (P.rep (P.rule2 (P.expect TokenKind.Comma ignore) pNumber (fun _ n -> n)))
        (fun head tail -> head :: tail))
      (fun opt ->
        match opt with
        | Some it -> it
        | None -> [])

  let private sGrammarLazy: Lazy<P.Grammar<Token, int list>> = lazy (P.build pSeq [])

  let private tokenize (text: string) : Token array =
    let array = ResizeArray()
    let mutable i = 0

    for word in text.Split(",") do
      let word = word.Trim()

      if word <> "" then
        if i <> 0 then array.Add(Token.Comma)

        array.Add(Token.Number(int word))
        i <- i + 1

    array.ToArray()

  let internal parseString (text: string) : int list =
    let tokenArray = tokenize text
    P.parseArray TokenKind.ofToken tokenArray sGrammarLazy.Value

  let internal tests () =
    let p s x =
      let actual =
        try
          parseString s |> sprintf "%A"
        with
        | ex -> sprintf "ERROR: %A" ex

      if actual = x then
        true
      else
        eprintfn "Assertion violated:\nactual: '%s'\nexpected: '%s'" actual x
        false

    assert (p "" "[]")
    assert (p "0" "[0]")
    assert (p "0, 1, 2" "[0; 1; 2]")

// ミニ言語のパーサー
module private MiniLang =
  module P = ParserCombinator

  (*
    BNF-like Grammar

    Legends:
      T? = T | ε
      T+ = μM. (T | M T)
      T* = (T+)?
      T sepBy1(S) = μM. (T | T S M)
      T sepBy0(S) = (T sepBy1(S))?
      T by1(S) = (μM. (T | T S M)) S?
      T by0(S) = (T by(S))?

    Literal =
      '(' ')'
    | 'false' | 'true'
    | 'number_literal'
    | 'string_literal'

    Ty =
      'ident'
    | '(' Ty by1(',') ')'

    Pat =
      Literal
    | '_'
    | '(' Pat by1(',') ')'
    | Pat '|' Pat

    Expr =
      Literal
    | 'ident'
    | '(' Expr by1(',') ')'
    | Expr '(' Expr by0(',') ')'
    | Expr '[' Expr ']'
    | Expr '.' 'ident'
    | 'prefix_op' Expr
    | Expr 'infix_op' Expr
    | 'break' | 'continue' | 'return' Expr?
    | Block | IfExpr | LoopExpr
    where
      IfExpr =
        'if' Expr Block ('else' (IfExpr | Paren | Block))?
      MatchExpr =
        'match' Expr '{'
          (Pat '=>' (Block | Expr ','))*
        '}'
      LoopExpr =
        'loop' Block

    Stmt =
      Expr ';'
    | 'let' Pat '=' (Block | Expr) ';'

    Item =
      TypeItem
    | FnItem
    where
      TypeItem =
        'enum' 'ident'
        '{' ('ident' ('(' Ty ')')?)) by0(',') '}'
      | 'struct' 'ident'
        '{' ('ident' ':' Ty) by0(',') '}'

      FnItem =
      | 'fn' 'ident'
        '(' ('ident' ':' Ty) by0(',') ')'
        ('->' Ty)?
        Block

    Block =
      '{' (Item | Stmt)* Expr '}'

    Root = Item*
  *)

  [<RequireQualifiedAccess; NoEquality; NoComparison>]
  type internal Token =
    | Bad
    | Spaces
    | Number of int
    | Ident of string
    // Keywords:
    | Break
    | Continue
    | Else
    | Enum
    | Fn
    | For
    | If
    | Let
    | Loop
    | Match
    | Return
    | Struct
    | Underscore
    | While
    // Brackets:
    | LeftParen
    | RightParen
    | LeftBracket
    | RightBracket
    | LeftBrace
    | RightBrace
    // Sigils:
    | Bang
    | BangEqual
    | Colon
    | Comma
    | Equal
    | EqualEqual
    | LeftAngle
    | Minus
    | MinusEqual
    | Percent
    | Plus
    | PlusEqual
    | RightFatArrow
    | Semi
    | Slash
    | Star

    | False
    | True
    | Dot
    | RightThinArrow

  /// トークンの種類を表す適当な定数
  module private TokenKind =
    let Bad = 1
    let Number = 2
    let Ident = 3
    // Keywords:
    let Break = 4
    let Continue = 5
    let Else = 6
    let Enum = 7
    let Fn = 8
    let For = 9
    let If = 10
    let Let = 11
    let Loop = 12
    let Match = 13
    let Return = 14
    let Struct = 15
    let Underscore = 16
    let While = 17
    // Brackets:
    let LeftParen = 18
    let RightParen = 19
    let LeftBracket = 20
    let RightBracket = 21
    let LeftBrace = 22
    let RightBrace = 23
    // Sigils:
    let Bang = 24
    let BangEqual = 25
    let Colon = 26
    let Comma = 27
    let Equal = 28
    let EqualEqual = 29
    let LeftAngle = 30
    let Minus = 31
    let MinusEqual = 32
    let Percent = 33
    let Plus = 34
    let PlusEqual = 35
    let RightFatArrow = 36
    let Semi = 37
    let Slash = 38
    let Star = 39

    let False = 40
    let True = 41
    let Dot = 42
    let Spaces = 43
    let RightThinArrow = 44

    let ofToken token =
      match token with
      | Token.Bad -> Bad
      | Token.Number _ -> Number
      | Token.Ident _ -> Ident
      // Keywords:
      | Token.Break -> Break
      | Token.Continue -> Continue
      | Token.Else -> Else
      | Token.Enum -> Enum
      | Token.Fn -> Fn
      | Token.For -> For
      | Token.If -> If
      | Token.Let -> Let
      | Token.Loop -> Loop
      | Token.Match -> Match
      | Token.Return -> Return
      | Token.Struct -> Struct
      | Token.Underscore -> Underscore
      | Token.While -> While
      // Brackets:
      | Token.LeftParen -> LeftParen
      | Token.RightParen -> RightParen
      | Token.LeftBracket -> LeftBracket
      | Token.RightBracket -> RightBracket
      | Token.LeftBrace -> LeftBrace
      | Token.RightBrace -> RightBrace
      // Sigils:
      | Token.Bang -> Bang
      | Token.BangEqual -> BangEqual
      | Token.Colon -> Colon
      | Token.Comma -> Comma
      | Token.Equal -> Equal
      | Token.EqualEqual -> EqualEqual
      | Token.LeftAngle -> LeftAngle
      | Token.Minus -> Minus
      | Token.MinusEqual -> MinusEqual
      | Token.Percent -> Percent
      | Token.Plus -> Plus
      | Token.PlusEqual -> PlusEqual
      | Token.RightFatArrow -> RightFatArrow
      | Token.Semi -> Semi
      | Token.Slash -> Slash
      | Token.Star -> Star

      | Token.False -> False
      | Token.True -> True
      | Token.Dot -> Dot
      | Token.Spaces -> Spaces
      | Token.RightThinArrow -> RightThinArrow

    let internal asKeyword s =
      match s with
      | "break" -> Token.Break
      | "continue" -> Token.Continue
      | "else" -> Token.Else
      | "enum" -> Token.Enum
      | "false" -> Token.False
      | "fn" -> Token.Fn
      | "for" -> Token.For
      | "if" -> Token.If
      | "let" -> Token.Let
      | "loop" -> Token.Loop
      | "match" -> Token.Match
      | "return" -> Token.Return
      | "struct" -> Token.Struct
      | "true" -> Token.True
      | "underscore" -> Token.Underscore
      | "while" -> Token.While
      | _ -> Token.Ident s

  [<RequireQualifiedAccess; NoEquality; NoComparison>]
  type internal Unary =
    | Minus
    | Not

  [<RequireQualifiedAccess; NoEquality; NoComparison>]
  type internal Binary =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulo
    | Equal
    | NotEqual
    | LessThan

  [<RequireQualifiedAccess; NoEquality; NoComparison>]
  type internal BinaryAssign =
    | Assign
    | AddAssign
    | SubtractAssign

  [<RequireQualifiedAccess; NoEquality; NoComparison>]
  type internal Literal =
    | Unit
    | Bool of bool
    | Int of int

  [<RequireQualifiedAccess; NoEquality; NoComparison>]
  type internal Ty =
    | Ident of string
    | Tuple of Ty list

  and [<RequireQualifiedAccess; NoEquality; NoComparison>] internal Pat =
    | Ident of string
    | Literal of Literal
    | Or of Pat * Pat

  and [<RequireQualifiedAccess; NoEquality; NoComparison>] internal Expr =
    | Ident of string
    | Literal of Literal
    | Tuple of Expr * Expr * Expr list
    | Call of Expr * Expr list
    | Index of Expr * Expr
    | Field of Expr * string
    | UniOp of Unary * Expr
    | BinOp of Binary * Expr * Expr
    | Assign of BinaryAssign * Expr * Expr
    | Block of Block
    | Break
    | Continue
    | Return of Expr option
    | If of cond: Expr * thenClause: Expr * elseClause: Expr option
    | Loop of Block

  and [<RequireQualifiedAccess; NoEquality; NoComparison>] internal Stmt =
    | Expr of Expr
    | Let of Pat * Expr

  and [<RequireQualifiedAccess; NoEquality; NoComparison>] internal Item =
    | FnItem of FnItem
    | EnumItem of EnumItem
    | StructItem of StructItem

  and [<RequireQualifiedAccess; NoEquality; NoComparison>] internal FnItem =
    { Name: string
      Params: (string * Ty) list
      ResultOpt: Ty option
      Body: Block }

  and [<RequireQualifiedAccess; NoEquality; NoComparison>] internal EnumItem =
    { Name: string
      Variants: (string * Ty option) list }

  and [<RequireQualifiedAccess; NoEquality; NoComparison>] internal StructItem =
    { Name: string
      Fields: (string * Ty) list }

  and [<RequireQualifiedAccess; NoEquality; NoComparison>] internal Block =
    { Items: Item list
      Stmts: Stmt list
      Expr: Expr option }

  [<RequireQualifiedAccess; NoEquality; NoComparison>]
  type internal Root = { Items: Item list }

  // 1+ items separated by sep and may dangle sep
  let private by1 pItem pSep =
    let wrap p = P.rule1 p List.singleton

    P.rule2 (P.infixLeft (wrap pItem) pSep (wrap pItem) (fun l _ r -> l @ r)) (P.opt pSep ignore) (fun items _ -> items)

  // 0+ items separated by sep and may dangle sep unless empty
  let private by0 pItem pSep =
    P.opt (by1 pItem pSep) (Option.defaultValue [])

  let private pTyRec: P.RecRule<_, Ty> = P.recursive "Type"
  // let private pPatRec: P.RecRule<_, Pat> = P.recursive "Pattern"
  let private pExprRec: P.RecRule<_, Expr> = P.recursive "Expression"
  let private pStmtRec: P.RecRule<_, Stmt> = P.recursive "Statement"
  let private pItemRec: P.RecRule<_, Item> = P.recursive "Item"

  let private pRecTy = P.recurse pTyRec
  // let private pRecPat = P.recurse pPatRec
  let private pRecExpr = P.recurse pExprRec
  let private pRecStmt = P.recurse pStmtRec
  let private pRecItem = P.recurse pItemRec

  let private lp = P.expect TokenKind.LeftParen ignore
  let private rp = P.expect TokenKind.RightParen ignore
  let private lb = P.expect TokenKind.LeftBracket ignore
  let private rb = P.expect TokenKind.RightBracket ignore
  let private lc = P.expect TokenKind.LeftBrace ignore
  let private rc = P.expect TokenKind.RightBrace ignore
  let private colon = P.expect TokenKind.Colon ignore
  let private comma = P.expect TokenKind.Comma ignore
  let private semi = P.expect TokenKind.Semi ignore

  let private pIdent =
    P.expect TokenKind.Ident (fun (Token.Ident it | Unreachable it) -> it)

  let private pLiteral =
    P.choice [ // P.rule2 lp rp (fun _ _ -> Literal.Unit)

               P.expect TokenKind.Number (fun (Token.Number value | Unreachable value) -> Literal.Int value)
               P.expect TokenKind.False (fun _ -> Literal.Bool false)
               P.expect TokenKind.True (fun _ -> Literal.Bool true) ]

  // `Stmt* Expr` をパースする。ただしこれは曖昧なので以下の構文に変換する
  // `μX. Expr (; X)? | Stmt X`
  let private pBlockItems: P.Rule<_, Stmt list * Expr option> =
    P.fix "BlockItems" (fun pSelf ->
      P.choice [ P.rule2 pRecExpr (P.opt (P.rule2 semi pSelf (fun _ result -> result)) id) (fun expr restOpt ->
                   match restOpt with
                   | Some (stmts, exprOpt) -> Stmt.Expr expr :: stmts, exprOpt
                   | None -> [], Some expr)
                 P.rule2 pRecStmt pSelf (fun stmt (stmts, exprOpt) -> stmt :: stmts, exprOpt) ])

  let private pBlock =
    P.rule3 lc pBlockItems rc (fun _ (stmts, exprOpt) _ ->
      ({ Items = []
         Stmts = stmts
         Expr = exprOpt }: Block))

  let private pTy1 =
    P.choice [ P.rule1 pIdent Ty.Ident

               //  P.rule2 lp rp (fun _ _ -> Ty.Tuple [])
               //  P.rule3 lp pRecTy rp (fun _ it _ -> it)

               P.rule2
                 lp
                 (P.choice [ P.rule1 rp (fun _ -> Ty.Tuple [])
                             P.rule2 pRecTy rp (fun it _ -> it) ])
                 (fun _ it -> it) ]

  let private pPrimary =
    P.choice [ P.rule1 pLiteral Expr.Literal
               P.rule1 pIdent Expr.Ident

               // P.rule3 lp pRecExpr rp (fun _ e _ -> e)

               P.rule2
                 lp
                 (P.choice [ P.rule1 rp (fun _ -> Expr.Literal Literal.Unit)
                             P.rule2 pRecExpr rp (fun e _ -> e) ])
                 (fun _ it -> it)

                ]

  let private pSuffix =
    P.infixLeft
      pPrimary
      (P.choice [ P.rule2 (P.expect TokenKind.Dot ignore) pIdent (fun _ field lhs -> Expr.Field(lhs, field))
                  P.rule3 lb pRecExpr rb (fun _ index _ lhs -> Expr.Index(lhs, index)) ])
      (P.eps ())
      (fun lhs mid _ -> mid lhs)

  let private pPrefix =
    P.choice [ P.rule2
                 (P.choice [ P.expect TokenKind.Minus (fun _ -> Unary.Minus)
                             P.expect TokenKind.Bang (fun _ -> Unary.Not) ])
                 pSuffix
                 (fun op arg -> Expr.UniOp(op, arg))
               pSuffix ]

  let private pMul =
    P.infixLeft
      pPrefix
      (P.choice [ P.cut TokenKind.Star (fun _ -> Binary.Multiply)
                  P.cut TokenKind.Slash (fun _ -> Binary.Divide) ])
      pPrefix
      (fun l op r -> Expr.BinOp(op, l, r))

  let private pAdd =
    P.infixLeft
      pMul
      (P.choice [ P.cut TokenKind.Plus (fun _ -> Binary.Add)
                  P.cut TokenKind.Minus (fun _ -> Binary.Subtract) ])
      pMul
      (fun l op r -> Expr.BinOp(op, l, r))

  let private pAssign =
    P.rule2 pAdd (P.opt (P.rule2 (P.expect TokenKind.Equal ignore) pRecExpr (fun _ e -> e)) id) (fun l rOpt ->
      match rOpt with
      | Some r -> Expr.Assign(BinaryAssign.Assign, l, r)
      | None -> l)

  let private pIfExpr =
    P.rule4
      (P.expect TokenKind.If ignore)
      pRecExpr
      pBlock
      (P.opt (P.rule2 (P.expect TokenKind.Else ignore) pBlock (fun _ a -> Expr.Block a)) id)
      (fun _ c b a -> Expr.If(c, Expr.Block b, a))

  let private pExpr1 = P.choice [ pAssign; pIfExpr ]

  let private pStmt1 =
    let pExprStmt = P.rule2 pRecExpr semi (fun e _ -> Stmt.Expr e)

    // let ;
    let pLetStmt =
      P.rule2 (P.expect TokenKind.Let ignore) semi (fun _ _ ->
        Stmt.Let(Pat.Literal Literal.Unit, Expr.Literal Literal.Unit))

    P.choice [ pExprStmt; pLetStmt ]

  let private pParamList =
    let pParam = P.rule3 pIdent colon pRecTy (fun name _ ty -> name, ty)

    P.rule3 lp (by0 pParam comma) rp (fun _ it _ -> it)

  let private pItem1 =
    P.rule5
      (P.expect TokenKind.Fn ignore)
      pIdent
      pParamList
      (P.opt (P.rule2 (P.expect TokenKind.RightThinArrow ignore) pRecTy (fun _ ty -> ty)) id)
      pBlock
      (fun _ name paramList resultOpt body ->
        ({ Name = name
           Params = paramList
           ResultOpt = resultOpt
           Body = body }: FnItem))
    |> (fun p -> P.rule1 p Item.FnItem)

  let private pRoot =
    P.rule1 (P.rep pRecItem) (fun items -> ({ Items = items }: Root))

  let private sGrammarLazy: Lazy<P.Grammar<Token, Root>> =
    lazy
      (P.build
        pRoot
        [ P.bind pTyRec pTy1

          // P.bind pPatRec pRecPat

          P.bind pExprRec pExpr1
          P.bind pStmtRec pStmt1
          P.bind pItemRec pItem1 ])

  module internal Tokenizer =
    let private punctuations =
      [| "(", Token.LeftParen
         ")", Token.RightParen
         "[", Token.LeftBracket
         "]", Token.RightBracket
         "{", Token.LeftBrace
         "}", Token.RightBrace

         "!", Token.Bang
         "!=", Token.BangEqual
         ":", Token.Colon
         ",", Token.Comma
         "=", Token.Equal
         "==", Token.EqualEqual
         "<", Token.LeftAngle
         "-", Token.Minus
         "-=", Token.MinusEqual
         "%", Token.Percent
         "+", Token.Plus
         "+=", Token.PlusEqual
         "=>", Token.RightFatArrow
         ";", Token.Semi
         "/", Token.Slash
         "*", Token.Star

         ".", Token.Dot
         "->", Token.RightThinArrow |]
      |> Array.sortByDescending fst

    let private isDigit (c: char) = '0' <= c && c <= '9'
    let private isUpper (c: char) = 'A' <= c && c <= 'Z'
    let private isLower (c: char) = 'a' <= c && c <= 'z'
    let private isLetter c = isUpper c || isLower c || c = '_'
    let private isSpace c = c = ' ' || c = '\r' || c = '\n'

    let internal tokenize (s: string) =
      let mutable tokens = ResizeArray()
      let mutable index = 0

      while index < s.Length do
        if isSpace s.[index] then
          index <- index + 1
        else if isLetter s.[index] then
          let start = index

          while index < s.Length
                && (isLetter s.[index] || isDigit s.[index]) do
            index <- index + 1

          let token = TokenKind.asKeyword s.[start .. index - 1]
          tokens.Add(token)
        else if isDigit s.[index] then
          let start = index

          while index < s.Length && isDigit s.[index] do
            index <- index + 1

          tokens.Add(Token.Number(int s.[start .. index - 1]))
        else
          match punctuations
                |> Array.tryFindIndex (fun (p, _) -> s.[index .. index + p.Length - 1] = p)
            with
          | Some i ->
            let p, t = punctuations.[i]
            index <- index + p.Length
            tokens.Add(t)

          | None -> failwithf "Unrecognized character %A at %d" s.[index] index

      tokens.ToArray()

  let internal parseString (text: string) : Root =
    let tokenArray = Tokenizer.tokenize text
    P.parseV2 TokenKind.ofToken tokenArray sGrammarLazy.Value

  let internal tests () =
    let display (root: Root) = sprintf "%A" root

    let p s expected =
      let actual = parseString s |> display

      if actual = expected then
        true
      else
        eprintfn "actual: %s\nexpected: %s" actual expected
        false

    assert (p "fn f(x: int, y: string) -> () { 0; let; 1 + 2 * 3 }" "Unit")

let v2 () = MiniLang.tests ()
// Arith.tests ()
// NumberSequence.tests ()
