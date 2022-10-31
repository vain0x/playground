module internal ParserV2

open System
open Util

module internal ParserCombinator =
  /// Kind of token.
  type private K = int

  type private IParseContext =
    abstract Shift: unit -> obj
    abstract Push: obj -> unit
    abstract Pop: unit -> obj
    abstract OnBegin: unit -> int
    abstract OnEnd: int -> unit

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

    | Mu of local: Symbol * body: Term

  and [<ReferenceEqualityAttribute; NoComparisonAttribute; StructuredFormatDisplay("&{name}")>] private Symbol =
    private | Symbol of id: obj * name: string * Lazy<Term>

  [<Struct; NoEquality; NoComparison>]
  type Rule<'T, 'N> = private Rule of Term

  [<NoEquality; NoComparison>]
  type RecRule<'T, 'N> = private RecRule of Symbol * termRef: Term option ref

  [<Struct; NoEquality; NoComparison>]
  type Binding = private Binding of id: obj * name: string * Term

  exception ParseError of msg: string * index: int with
    override this.ToString() =
      sprintf "Parse Error: %s\n%s" this.msg this.StackTrace

  exception private ExpectedTokenError of Token: K * Index: int
  exception private ChoiceError of Tokens: K list * Index: int

  [<NoEquality; NoComparison>]
  type GrammarData =
    private
      { Start: Symbol
        RuleArray: Symbol array
        RuleMemo: HashMap<obj, int>
        NullableMemo: HashMap<obj, bool>
        FirstSetMemo: HashMap<obj, Set<K>>
        LeftRecMemo: HashMap<Term, Set<K>>
        JumpMemo: HashMap<Term, HashMap<K, Term>>
        FallbackMemo: HashMap<Term, Term> }

  type Grammar<'T, 'N> = Grammar of GrammarData

  // ---------------------------------------------
  // Combinators
  // ---------------------------------------------

  // tokens:

  // let look (token: int) : Rule<'T, unit> = todo ()

  /// 特定の種類のトークンが1つ出現することを表す規則
  let expect (token: K) (extractor: 'T -> 'N) : Rule<'T, 'N> =
    Rule(Term.Expect(token, false, (fun ctx -> ctx.Push(extractor (ctx.Shift() :?> 'T) :> obj))))

  /// 特定の種類のトークンが1つ出現することを表す規則
  /// また、カットを行う
  ///
  /// - カットとは、`choice` オペレータによる構文規則の選択を確定させるということ
  ///   すなわち `choice` は先読みによりこのトークンが出現するか確認して、出現していたらこの `cut` を含む規則を選択する
  let cut (token: K) (extractor: 'T -> 'N) : Rule<'T, 'N> =
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
  let recurse (recRule: RecRule<'T, 'N>) : Rule<'T, 'N> =
    let (RecRule (symbol, _)) = recRule
    Rule(Term.Symbol symbol)

  /// 再帰的な規則に定義を束縛することを表す
  let bind (recRule: RecRule<'T, 'N>) (actualRule: Rule<'T, 'N>) : Binding =
    let (RecRule (symbol, termRef)) = recRule
    let (Symbol (id, name, _)) = symbol
    let (Rule actualTerm) = actualRule

    assert (termRef.contents |> Option.isNone)
    termRef.contents <- Some actualTerm

    Binding(id, name, actualTerm)

  /// μ-operator
  ///
  /// `μX. F(X)` is equivalent to `X` with a rule `X → F(X)`.
  let mu (name: string) (creator: Rule<'T, 'N> -> Rule<'T, 'N>) : Rule<'T, 'N> =
    let recRule = recursive name
    let (Rule term) = creator (recurse recRule)

    let (RecRule (symbol, termRef)) = recRule
    termRef.contents <- Some term

    Rule(Term.Symbol symbol)

  // let label (name: string) (rule: Rule<'T, 'N>) : Rule<'T, 'N> = todo ()

  // combinators:

  /// トークンを消費しない規則
  let eps<'T, 'N> (value: 'N) : Rule<'T, 'N> =
    Rule(Term.Eps(fun ctx -> ctx.Push value))

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
  let rule2 (r1: Rule<'T, 'A>) (r2: Rule<'T, 'B>) (decoder: 'A -> 'B -> 'N) : Rule<'T, 'N> =
    let (Rule r1) = r1
    let (Rule r2) = r2

    Term.Seq(
      [ r1; r2 ],
      fun ctx ->
        let n2 = ctx.Pop() :?> 'B
        let n1 = ctx.Pop() :?> 'A
        ctx.Push(decoder n1 n2 :> obj)
    )
    |> Rule

  /// 3つの規則の並び
  let rule3 (r1: Rule<'T, 'A>) (r2: Rule<'T, 'B>) (r3: Rule<'T, 'C>) (decoder: 'A -> 'B -> 'C -> 'N) : Rule<'T, 'N> =
    let (Rule r1) = r1
    let (Rule r2) = r2
    let (Rule r3) = r3

    Term.Seq(
      [ r1; r2; r3 ],
      fun ctx ->
        let n3 = ctx.Pop() :?> 'C
        let n2 = ctx.Pop() :?> 'B
        let n1 = ctx.Pop() :?> 'A
        ctx.Push(decoder n1 n2 n3 :> obj)
    )
    |> Rule

  /// 4つの規則の並び
  let rule4
    (r1: Rule<'T, 'A>)
    (r2: Rule<'T, 'B>)
    (r3: Rule<'T, 'C>)
    (r4: Rule<'T, 'D>)
    (decoder: 'A -> 'B -> 'C -> 'D -> 'N)
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
        ctx.Push(decoder n1 n2 n3 n4 :> obj)
    )
    |> Rule

  let rule5 r1 r2 r3 r4 r5 decoder =
    rule4 r1 r2 r3 (rule2 r4 r5 (fun n4 n5 -> n4, n5)) (fun n1 n2 n3 (n4, n5) -> decoder n1 n2 n3 n4 n5)

  let rule6 r1 r2 r3 r4 r5 r6 decoder =
    rule4 r1 r2 r3 (rule3 r4 r5 r6 (fun n4 n5 n6 -> n4, n5, n6)) (fun n1 n2 n3 (n4, n5, n6) -> decoder n1 n2 n3 n4 n5 n6)

  let rule7 r1 r2 r3 r4 r5 r6 r7 decoder =
    rule4 r1 r2 r3 (rule4 r4 r5 r6 r7 (fun n4 n5 n6 n7 -> n4, n5, n6, n7)) (fun n1 n2 n3 (n4, n5, n6, n7) ->
      decoder n1 n2 n3 n4 n5 n6 n7)

  /// 複数の規則のうち、適用可能なものを1つ選択する規則
  ///
  /// - `rules` は適用可能な規則の候補からなるリストである
  ///     - それぞれの候補はカットを含む規則でなければいけない
  let choice (rules: Rule<'T, 'N> list) : Rule<'T, 'N> =
    Rule(Term.Choice(List.map (fun (Rule r) -> r) rules))

  /// 省略可能な規則
  let opt (rule: Rule<'T, 'A>) (decoder: ('A option -> 'N)) : Rule<'T, 'N> =
    choice [ rule1 rule (fun a -> decoder (Some a))
             eps (decoder None) ]

  let leftRec (left: Rule<'T, 'N>) (right: Rule<'T, 'A>) (decoder: 'N -> 'A -> 'N) : Rule<'T, 'N> =
    let (Rule left) = left
    let (Rule right) = right

    Rule(
      Term.LeftRec(
        left,
        right,
        (fun ctx ->
          let right = ctx.Pop() :?> 'A
          let left = ctx.Pop() :?> 'N
          ctx.Push(decoder left right :> obj))
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
  let rep (item: Rule<'T, 'N>) : Rule<'T, 'N list> =
    let (Rule item) = item
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
  // LeftRecDetection
  // ---------------------------------------------

  let inline private equalsByRef (x: obj) (y: obj) = Object.ReferenceEquals(x, y)

  let private leftRecDetectionWith (isNullable: Term -> bool) (memo: HashMap<obj, bool>) (symbol: Symbol) =
    let mutable current: obj = null
    let status = HashMap()

    let rec detectRec term =
      match term with
      | Term.Expect _
      | Term.Eps _ -> ()

      | Term.Symbol symbol
      | Term.Mu (symbol, _) ->
        let (Symbol (id, name, termLazy)) = symbol

        if memo.ContainsKey(id) then
          let parent = current
          current <- id

          status.Add(id, false)
          detectRec termLazy.Value
          status.[id] <- true

          current <- parent
        else
          match HashMap.tryFind id status with
          | Some false ->
            // Detected!
            status.[id] <- true
            let direct = equalsByRef current id
            memo.Add(id, direct)
            eprintfn "trace: Symbol %s is%s left-recursive)" name (if direct then " directly" else "")

          | Some true -> ()

          | None ->
            let parent = current
            current <- id

            status.Add(id, false)
            detectRec termLazy.Value
            status.[id] <- true

            if memo.ContainsKey(id) |> not then
              eprintfn "trace: Symbol %s isn't left-recursive" name

            current <- parent

      | Term.Map (t, _) -> detectRec t

      | Term.Seq (terms, _) ->
        let rec seqLoop terms =
          match terms with
          | t :: terms ->
            detectRec t
            if isNullable t then seqLoop terms

          | [] -> ()

        seqLoop terms

      | Term.Choice branches -> List.iter detectRec branches

      | Term.LeftRec (left, right, _) -> if isNullable left then detectRec right

    detectRec (Term.Symbol symbol)

  // ---------------------------------------------
  // LeftRecElimination
  // ---------------------------------------------

  let private leftRecElimination (term: Term) =
    let rec elimRec term =
      match term with
      | Term.Expect _
      | Term.Symbol _
      | Term.Eps _ -> term

      | Term.Map (t, action) -> Term.Map(elimRec t, action)
      | Term.Seq (terms, action) -> Term.Seq(List.map elimRec terms, action)
      | Term.Choice branches -> Term.Choice(List.map elimRec branches)

      | Term.LeftRec (left, right, action) ->
        let left = elimRec left
        let right = elimRec right

        // leftRec(L, R)
        //    ==> L (R action)*
        //    ==> L (mu X. (R action X)?)
        let (RecRule (symbol, termRef)) = recursive "leftRec"

        let body =
          Term.Choice [ Term.Seq(
                          [ Term.Map(right, action)
                            Term.Symbol symbol ],
                          ignore
                        )
                        Term.Eps ignore ]

        termRef.contents <- Some body

        Term.Seq([ left; Term.Symbol symbol ], ignore)

      | Term.Mu (symbol, body) -> Term.Mu(symbol, elimRec body)

    elimRec term

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
    | Term.Mu (symbol, _) -> LTerm([], LTermData.Single(LSingle.Symbol symbol), [])

    | Term.Eps action -> LTerm([], LTermData.Eps, [ action ])

    | Term.Map (t, action) ->
      eprintfn "trace: leftUp map"
      let (LTerm (preActions, t, actions)) = leftUp t
      LTerm(preActions, t, List.append actions [ action ])

    | Term.Seq (ts, action) ->
      eprintfn "trace: leftUp seq"
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
      eprintfn "todo: skip leftRec"
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

  // ---------------------------------------------
  // Build
  // ---------------------------------------------

  let private computeNullableWith (nullableMemo: HashMap<_, bool>) (term: Term) =
    let rec nullableRec term =
      match term with
      | Term.Expect _ -> false
      | Term.Eps _ -> true

      | Term.Symbol (Symbol (id, _, _))
      | Term.Mu (Symbol (id, _, _), _) -> HashMap.findOr id false nullableMemo

      | Term.Map (t, _) -> nullableRec t
      | Term.Seq (terms, _) -> terms |> List.forall nullableRec
      | Term.Choice terms -> terms |> List.exists nullableRec
      | Term.LeftRec (left, _, _) -> nullableRec left

    nullableRec term

  let private computeFirstSetWith (nullableMemo: HashMap<obj, bool>, firstSetMemo: HashMap<obj, Set<K>>) (term: Term) =
    let rec firstRec term =
      match term with
      | Term.Expect (k, _, _) -> set [ k ]

      | Term.Symbol (Symbol (id, _, _))
      | Term.Mu (Symbol (id, _, _), _) -> HashMap.findOr id Set.empty firstSetMemo

      | Term.Eps _ -> Set.empty
      | Term.Map (t, _) -> firstRec t
      | Term.Seq (terms, _) -> firstManyRec terms
      | Term.Choice terms -> terms |> List.map firstRec |> Set.unionMany
      | Term.LeftRec (left, right, _) -> firstManyRec [ left; right ]

    and firstManyRec terms =
      match terms with
      | [] -> Set.empty

      | t :: terms ->
        if computeNullableWith nullableMemo t then
          Set.union (firstRec t) (firstManyRec terms)
        else
          firstRec t

    firstRec term

  let private doBuild (start: Symbol) (bindings: Binding list) : GrammarData =
    // indexing
    let ruleArray = ResizeArray()
    let ruleRev = HashMap()

    let dummy = Symbol(obj (), "dummy", lazy (Term.Choice []))

    ruleArray.Add(dummy)
    ruleRev.Add(dummy :> obj, 0)

    let symbolIdOf (symbol: Symbol) =
      let (Symbol (id, _, _)) = symbol
      id

    let indexOf (symbol: Symbol) =
      let id = symbolIdOf symbol

      if ruleRev.ContainsKey(id) |> not then
        failwithf "Rule not indexed (id:%A, rule:%A)" id symbol

      ruleRev.[id]

    let intern (rule: Symbol) = ruleArray.[indexOf rule]

    (let rec internSymbol symbol =
      let (Symbol (id, name, termLazy)) = symbol

      match HashMap.tryFind id ruleRev with
      | Some i ->
        // ruleArray.[i]
        ()

      | None ->
        // let termRef = ref None

        // let symbol =
        //   Symbol(id, name, lazy (termRef.contents |> Option.get))

        let index = ruleArray.Count
        ruleArray.Add(symbol)
        ruleRev.Add(id, index)

        indexingRec termLazy.Value
     // termRef.contents <- Some()
     // symbol

     and indexingRec (term: Term) =
       match term with
       | Term.Expect _
       | Term.Eps _ -> ()

       | Term.Symbol symbol
       | Term.Mu (symbol, _) -> internSymbol symbol

       | Term.Map (t, _) -> indexingRec t
       | Term.Seq (terms, _) -> List.iter indexingRec terms
       | Term.Choice branches -> List.iter indexingRec branches

       | Term.LeftRec (left, right, _) ->
         indexingRec left
         indexingRec right

     for Binding (id, name, term) in bindings do
       internSymbol (Symbol(id, name, lazy (term))))

    // dump
    (let rec hasChoice term =
      match term with
      | Term.Map (r, _) -> hasChoice r
      | Term.Seq (rules, _) -> rules |> List.exists hasChoice
      | Term.Choice _ -> true
      | _ -> false

     let rec dumpRec nl (term: Term) =
       match term with
       | Term.Symbol symbol
       | Term.Mu (symbol, _) ->
         let (Symbol (_, name, _)) = symbol
         sprintf "R%d:%s" (indexOf symbol) name

       | Term.Expect (token, _, _) -> sprintf "'%d'" token

       | Term.Eps _ -> "eps"

       | Term.Map (t, _) -> dumpRec nl t
       | Term.Seq (terms, _) ->
         sprintf
           "(%s)"
           (terms
            |> List.map (dumpRec nl)
            |> String.concat " ")

       | Term.Choice branches ->
         sprintf
           "(%s)"
           (branches
            |> List.map (dumpRec (nl + "  "))
            |> String.concat (nl + "| "))

       | Term.LeftRec (left, right, _) -> sprintf "leftRec(%s, %s)" (dumpRec nl left) (dumpRec nl right)

     for i in 1 .. ruleArray.Count - 1 do
       let rule = ruleArray.[i]

       let name, term =
         let (Symbol (_, name, termLazy)) = rule
         ":" + name, termLazy.Value

       let initial = if hasChoice term then "\n  " else " "
       eprintfn "R%d%s :=%s%s" i name initial (dumpRec "\n  " term))

    // for r in ruleArray do
    //   let (Symbol (_, _, termLazy)) = r
    //   eprintfn "leftUp: %A" (leftUp termLazy.Value)

    let nullableMemo = HashMap()
    let firstSetMemo = HashMap()
    let symbolFollowSetMemo = HashMap() // symbolId -> FOLLOW
    let dotFollowSetMemo = HashMap() // (term, dotIndex) -> FOLLOW

    // term -> (lookaheadKind -> branchTerm)
    // key must be Term.Choice
    let jumpMemo = HashMap()
    // term -> branchTerm
    let fallbackMemo = HashMap()

    // term -> rhsFirstSet
    // key must be Term.LeftRec
    let leftRecMemo = HashMap()

    // Compute nullable memo:
    (let mutable workList = Queue(ruleArray)
     let mutable nextList = Queue()
     let mutable modified = true

     while modified do
       modified <- false

       while workList.Count <> 0 do
         let (Symbol (id, name, termLazy)) as symbol = workList.Dequeue()
         assert (nullableMemo.ContainsKey(id) |> not)

         let nullable = computeNullableWith nullableMemo termLazy.Value

         eprintfn
           "trace: compute nullable %s: %s"
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

    // Compute first memo:
    (let mutable modified = true

     while modified do
       modified <- false

       for (Symbol (id, name, termLazy)) in ruleArray do
         let oldSet = HashMap.findOr id Set.empty firstSetMemo

         let firstSet = computeFirstSetWith (nullableMemo, firstSetMemo) termLazy.Value

         if firstSet <> oldSet then
           modified <- true
           eprintfn "trace: compute first %s: %A" name (List.ofSeq firstSet)

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
            let firstSet = computeFirstSetWith (nullableMemo, firstSetMemo) branchTerm

            if Set.isEmpty firstSet then
              if fallbackMemo.ContainsKey(term) then
                eprintfn "ambiguous: choice has multiple fallback paths: %A and %A" fallbackMemo.[term] branchTerm
                fallbackMemo.[term] <- branchTerm
              else
                fallbackMemo.Add(term, branchTerm)
            else
              for k in firstSet do
                if table.ContainsKey(k) then
                  eprintfn "ambiguous: choice %d-th branch: %A" k table.[k]
                else
                  table.Add(k, branchTerm)

          jumpMemo.Add(term, table)

      | Term.LeftRec (left, right, _) ->
        jumpRec left
        jumpRec right

      | Term.Eps _
      | Term.Expect _
      | Term.Symbol _
      | Term.Mu _ -> ()

      | Term.Map (t, _) -> jumpRec t
      | Term.Seq (terms, _) -> List.iter jumpRec terms

     for (Symbol (_, _, termLazy)) in ruleArray do
       jumpRec termLazy.Value)

    // Compute leftRecMemo:
    (let rec leftRecRec (term: Term) =
      match term with
      | Term.LeftRec (left, right, _) ->
        if leftRecMemo.ContainsKey(term) |> not then
          leftRecRec left
          leftRecRec right
          leftRecMemo.Add(term, computeFirstSetWith (nullableMemo, firstSetMemo) right)

          if computeNullableWith nullableMemo right then
            eprintfn "ambiguous: leftRec right mustn't nullable: %A" term

      | Term.Eps _
      | Term.Expect _
      | Term.Symbol _
      | Term.Mu _ -> ()

      | Term.Map (t, _) -> leftRecRec t
      | Term.Seq (terms, _) -> List.iter leftRecRec terms
      | Term.Choice terms -> List.iter leftRecRec terms

     for (Symbol (_, _, termLazy)) in ruleArray do
       leftRecRec termLazy.Value)

    // Perform left-rec detection:

    (let memo = HashMap()
     let isNullableFn = computeNullableWith nullableMemo

     for symbol in ruleArray do
       let (Symbol (id, _, _)) = symbol

       if memo.ContainsKey(id) |> not then
         leftRecDetectionWith isNullableFn memo symbol)

    // Compute FOLLOW sets:
    (let isNullable term = computeNullableWith nullableMemo term

     let getFirstSet term =
       computeFirstSetWith (nullableMemo, firstSetMemo) term

     let lookup (Symbol (id, _, _)) =
       HashMap.tryFind id symbolFollowSetMemo
       |> Option.defaultValue Set.empty

     let mutable modified = false // TODO: temporary disabled

     let merge followSet symbol =
       let oldSet = lookup symbol

       if Set.isSubset followSet oldSet |> not then
         symbolFollowSetMemo.Add(symbol, followSet)
         modified <- true

     let rec followRec current term =
       match term with
       | Term.Expect _
       | Term.Eps _ -> ()

       | Term.Symbol symbol
       | Term.Mu (symbol, _) -> merge current symbol

       | Term.Map (t, _) -> followRec current t
       | Term.Seq (terms, _) -> followSeqRec current terms
       | Term.Choice terms -> terms |> List.iter (followRec current)

       | Term.LeftRec (left, right, _) ->
         // FOLLOW(L) += FIRST(R) (since L R can appear)
         // FOLLOW(R) += FIRST(R) (since R R can appear)
         // FOLLOW(R) += k

         // Term.Choice [ left
         //               Term.Seq([ left; right ], ignore) ]

         // followSeqRec [ left; right ]
         todo ()

     and followSeqRec current terms =
       // heads is terms in reversed order
       // `current` is set of tokens that may follow heads
       let rec seqLoop current heads =
         match heads with
         | [] -> ()
         | [ t ] -> followRec current t

         | t :: heads ->
           followRec current t

           // follow of terms is: FIRST(T) | (if null(t) then k else {})
           let f =
             let f = getFirstSet t

             if isNullable t then
               Set.union f current
             else
               f

           seqLoop f heads

       seqLoop current (List.rev terms)

     while modified do
       modified <- false

       for symbol in ruleArray do
         let (Symbol (_, _, termLazy)) = symbol
         followRec (lookup symbol) termLazy.Value)

    ({ Start = intern start
       RuleArray = ruleArray.ToArray()
       RuleMemo = ruleRev
       NullableMemo = nullableMemo
       FirstSetMemo = firstSetMemo
       JumpMemo = jumpMemo
       LeftRecMemo = leftRecMemo
       FallbackMemo = fallbackMemo }: GrammarData)

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

  // ---------------------------------------------
  // Runtime
  // ---------------------------------------------

  // Deterministic by 1-token lookahead. Recursive decent.
  let parseV2<'T, 'N> (getKind: 'T -> K) (tokens: 'T array) (grammar: Grammar<'T, 'N>) : 'N =
    let (Grammar grammar) = grammar
    let nullableMemo = grammar.NullableMemo
    let firstSetMemo = grammar.FirstSetMemo
    let leftRecMemo = grammar.LeftRecMemo
    let jumpMemo = grammar.JumpMemo
    let fallbackMemo = grammar.FallbackMemo
    let termFirstSetMemo = HashMap()

    let getFirstSet term =
      match term with
      | Term.Symbol (Symbol (id, _, _))
      | Term.Mu (Symbol (id, _, _), _) -> grammar.FirstSetMemo.[id]

      | Term.Eps _ -> Set.empty

      | _ ->
        match HashMap.tryFind term termFirstSetMemo with
        | Some it -> it
        | None ->
          let result = computeFirstSetWith (nullableMemo, firstSetMemo) term

          termFirstSetMemo.[term] <- result
          result

    // runtime:

    let mutable index = 0
    let stack = Stack<obj>()

    let inline tokenAt i = tokens.[i]

    let shift cutting =
      assert (index < tokens.Length)
      let t = tokens.[index]
      eprintfn "trace: shift %d:%A%s" index t (if cutting then "!" else "")
      index <- index + 1
      t

    let near index =
      if index < tokens.Length then
        sprintf "%d:%A" index (tokenAt index)
      else
        "EOF"

    // Finds an example of token kind from input.
    let findWhat k =
      match tokens |> Array.tryFind (fun t -> getKind t = k) with
      | Some t -> sprintf "k:%d e.g. %A" k t
      | None -> sprintf "k:%d" k

    let ctx =
      { new IParseContext with
          override _.Shift() = shift false
          override _.Push(item) = stack.Push(item)
          override _.Pop() : obj = stack.Pop()

          // Stack balance check for debugging.
          override _.OnBegin() = stack.Count
          override _.OnEnd(previous: int) = assert (stack.Count = previous + 1) }

    let rec parseRec term : unit =
      match term with
      | Term.Expect (k, _, action) ->
        if index < tokens.Length then
          let t = tokenAt index

          if getKind t = k then
            action ctx
          else
            raise (ExpectedTokenError(k, index))
        else
          raise (ExpectedTokenError(k, index))

      | Term.Eps action -> action ctx

      | Term.Symbol symbol
      | Term.Mu (symbol, _) ->
        let (Symbol (_, name, termLazy)) = symbol
        let start = index

        try
          parseRec termLazy.Value
        with
        | _ ->
          eprintfn "trace: Error Trace - Parsing %s at %A" name (near start)
          reraise ()

      | Term.Map (t, action) ->
        parseRec t
        action ctx

      | Term.Seq (terms, action) ->
        for r in terms do
          parseRec r

        action ctx

      | Term.Choice branches ->
        let table =
          match HashMap.tryFind term jumpMemo with
          | Some it -> it
          | None -> failwithf "unreachable: jumpMemo missing: %A" term

        let onFallback () =
          let fallbackTerm =
            match HashMap.tryFind term fallbackMemo with
            | Some it -> it
            | None ->
              eprintfn "trace: choice doesn't have fallback: Choice[%A]" branches
              raise (ChoiceError(List.ofSeq table.Keys, index))

          parseRec fallbackTerm

        if index < tokens.Length then
          let token = tokenAt index
          let k = getKind token

          match HashMap.tryFind k table with
          | Some branchTerm ->
            (let index =
              List.tryFindIndex ((=) branchTerm) branches
              |> Option.defaultValue (-1)

             eprintfn "trace: choice k:%d index:%d" k index)

            parseRec branchTerm

          | None ->
            eprintfn "trace: choice falling back on k:%d" k
            onFallback ()
        else
          onFallback ()

      | Term.LeftRec (left, right, action) ->
        let rightFirstSet =
          match HashMap.tryFind term leftRecMemo with
          | Some it -> it
          | None -> failwithf "unreachable: leftRecMemo missed key %A" term

        let rec rightLoop () =
          // here left value (or accumulated value) is on the stack

          if index < tokens.Length then
            // try to parse rhs
            let token = tokenAt index
            let k = getKind token

            if Set.contains k rightFirstSet then
              parseRec right
              action ctx
              rightLoop ()

        parseRec left
        rightLoop ()

    try
      parseRec (Term.Symbol grammar.Start)
    with
    | ExpectedTokenError (k, index) ->
      let msg = sprintf "At %s, expected a token %s " (near index) (findWhat k)

      raise (ParseError(msg, index))

    | ChoiceError (kindList, index) ->
      let what =
        kindList
        |> List.sort
        |> List.map findWhat
        |> String.concat ", "

      let msg = sprintf "At %s, expected one of [%s]" (near index) what

      raise (ParseError(msg, index))

    if index <> tokens.Length then
      raise (ParseError(sprintf "error: unexpected EOF at %s" (near index), index))

    assert (stack.Count = 1)
    ctx.Pop() :?> 'N

  /// トークン列をパースする
  let parseArray (getKind: 'T -> K) (tokens: 'T array) (grammar: Grammar<'T, 'N>) : 'N = parseV2 getKind tokens grammar
