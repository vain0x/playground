module private ParserV1

type private HashMap<'K, 'T> = System.Collections.Generic.Dictionary<'K, 'T>

let inline private unreachable () = failwith "unreachable"
let inline private (|Unreachable|) _ = unreachable ()

module private ParserCombinator =
  /// Kind of token.
  type private K = int

  /// Term of syntax rule.
  [<RequireQualifiedAccess; ReferenceEquality; NoComparison>]
  type private Term<'T> =
    private
    | Expect of K * extractor: ('T -> obj)
    | Cut of K * extractor: ('T -> obj)

    | Symbol of Symbol<'T>
    | Eps of obj
    | Map of Term<'T> * mapping: (obj -> obj)
    | Seq of Term<'T> list * decode: (obj list -> obj)
    | Choice of Term<'T> list
    | InfixLeft of left: Term<'T> * mid: Term<'T> * right: Term<'T> * decode: (obj -> obj -> obj -> obj)

  and private Symbol<'T> = Symbol of id: obj * name: string * Lazy<Term<'T>>

  [<Struct; NoEquality; NoComparison>]
  type Rule<'T, 'N> = private Rule of Term<'T>

  [<NoEquality; NoComparison>]
  type RecRule<'T, 'N> = private RecRule of id: obj * name: string * rule: Rule<'T, 'N> option ref

  [<Struct; NoEquality; NoComparison>]
  type Binding<'T> = private Binding of id: obj * name: string * Term<'T>

  [<NoEquality; NoComparison>]
  type Parser<'T, 'N> =
    private
      { Start: Symbol<'T>
        RuleArray: Symbol<'T> array
        RuleMemo: HashMap<obj, int>
        Mapping: (obj -> 'N) }

  // tokens:

  // let look (token: int) : Rule<'T, unit> = todo ()

  /// 特定の種類のトークンが1つ出現することを表す規則
  let expect (token: int) (extractor: 'T -> 'N) : Rule<'T, 'N> =
    Rule(Term.Expect(token, extractor >> box))

  /// 特定の種類のトークンが1つ出現することを表す規則
  /// また、カットを行う
  ///
  /// - カットとは、`choice` オペレータによる構文規則の選択を確定させるということ
  ///   すなわち `choice` は先読みによりこのトークンが出現するか確認して、出現していたらこの `cut` を含む規則を選択する
  let cut (token: int) (extractor: 'T -> 'N) : Rule<'T, 'N> = Rule(Term.Cut(token, extractor >> box))

  // nominal rules:

  /// 再帰的な規則を作る
  ///
  /// - `recurse` により、定義される規則を他の規則の一部として利用できる
  /// - `build` する際に、再帰的な規則にその定義となる規則を `bind` したものを渡すこと
  ///     そうでなければエラーが発生する ("Rule xxx not bound")
  let recursive (name: string) : RecRule<_, _> =
    let r = ref None
    RecRule(r :> obj, name, r)

  /// 再帰的な規則の利用を表す規則
  let recurse (rule: RecRule<'T, 'N>) : Rule<'T, 'N> =
    let (RecRule (id, name, ruleRef)) = rule

    Symbol(
      id,
      name,
      lazy
        (match ruleRef.contents with
         | Some (Rule it) -> it
         | None -> failwithf "Rule '%s' unbound" name)
    )
    |> Term.Symbol
    |> Rule

  /// 再帰的な規則に定義を束縛することを表す
  let bind (recRule: RecRule<'T, 'N>) (actualRule: Rule<'T, 'N>) : Binding<'T> =
    let (RecRule (id, name, ruleRef)) = recRule
    ruleRef.contents <- Some actualRule

    let (Rule r) = actualRule
    Binding(id, name, r)

  // let label (name: string) (rule: Rule<'T, 'N>) : Rule<'T, 'N> = todo ()

  // combinators:

  // let eps () : Rule<'T, 'N> = todo ()

  /// 単一の規則とその変換
  let rule1 (r: Rule<'T, 'A>) (mapping: 'A -> 'N) : Rule<'T, 'N> =
    let (Rule r) = r

    Term.Map(r, (fun obj -> mapping (obj :?> 'A) :> obj))
    |> Rule

  /// 2つの規則の並び
  ///
  /// - `decode`: 規則のパース結果を結合する関数
  let rule2 (r1: Rule<'T, 'A>) (r2: Rule<'T, 'B>) (decode: 'A -> 'B -> 'N) : Rule<'T, 'N> =
    let (Rule r1) = r1
    let (Rule r2) = r2

    Term.Seq(
      [ r1; r2 ],
      fun objList ->
        match objList with
        | [ o1; o2 ] -> decode (o1 :?> 'A) (o2 :?> 'B) :> obj
        | _ -> unreachable ()
    )
    |> Rule

  /// 3つの規則の並び
  let rule3 (r1: Rule<'T, 'A>) (r2: Rule<'T, 'B>) (r3: Rule<'T, 'C>) (decode: 'A -> 'B -> 'C -> 'N) : Rule<'T, 'N> =
    let (Rule r1) = r1
    let (Rule r2) = r2
    let (Rule r3) = r3

    Term.Seq(
      [ r1; r2; r3 ],
      fun objList ->
        match objList with
        | [ o1; o2; o3 ] -> decode (o1 :?> 'A) (o2 :?> 'B) (o3 :?> 'C) :> obj
        | _ -> unreachable ()
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
      fun objList ->
        match objList with
        | [ o1; o2; o3; o4 ] -> decode (o1 :?> 'A) (o2 :?> 'B) (o3 :?> 'C) (o4 :?> 'D) :> obj
        | _ -> unreachable ()
    )
    |> Rule

  let rule5 r1 r2 r3 r4 r5 decode =
    rule4 r1 r2 r3 (rule2 r4 r5 (fun n4 n5 -> n4, n5)) (fun n1 n2 n3 (n4, n5) -> decode n1 n2 n3 n4 n5)

  let rule6 r1 r2 r3 r4 r5 r6 decode =
    rule4 r1 r2 r3 (rule3 r4 r5 r6 (fun n4 n5 n6 -> n4, n5, n6)) (fun n1 n2 n3 (n4, n5, n6) -> decode n1 n2 n3 n4 n5 n6)

  let rule7 r1 r2 r3 r4 r5 r6 r7 decode =
    rule4 r1 r2 r3 (rule4 r4 r5 r6 r7 (fun n4 n5 n6 n7 -> n4, n5, n6, n7)) (fun n1 n2 n3 (n4, n5, n6, n7) ->
      decode n1 n2 n3 n4 n5 n6 n7)

  /// 複数の規則のうち、適用可能なものを1つ選択する規則
  ///
  /// - `rules` は適用可能な規則の候補からなるリストである
  ///     - それぞれの候補はカットを含む規則でなければいけない
  let choice (rules: Rule<'T, 'N> list) : Rule<'T, 'N> =
    Rule(Term.Choice(List.map (fun (Rule r) -> r) rules))

  let opt (rule: Rule<'T, 'A>) (decode: ('A option -> 'N)) : Rule<'T, 'N> =
    choice [ rule1 rule (fun a -> decode (Some a))
             Rule(Term.Eps(decode None)) ]

  /// 左結合な二項演算の規則
  ///
  /// - 典型例としては乗算 (`E * E`) の式をパースするのに使う
  ///     - 仮に乗算の両辺の規則を `primary` とする
  ///     - 乗算の規則は `infixLeft primary (P.expect '*') primary (fun l _ r -> Mul(l, r))` のように書ける
  /// - `infixLeft l m r` はBNF風の記法で書けば以下の構文を表す
  ///     - `A = l | A m r`
  ///     - いわゆる「左再帰」の構文である。`choice` と `recurse` の組み合わせで書いてしまうと、パース処理が無限再帰に陥る
  let infixLeft
    (pLeft: Rule<'T, 'N>)
    (pMid: Rule<'T, 'A>)
    (pRight: Rule<'T, 'B>)
    (decode: 'N -> 'A -> 'B -> 'N)
    : Rule<'T, 'N> =
    let (Rule rLeft) = pLeft
    let (Rule rMid) = pMid
    let (Rule rRight) = pRight

    Rule(
      Term.InfixLeft(
        rLeft,
        rMid,
        rRight,
        (fun left mid right -> decode (left :?> 'N) (mid :?> 'A) (right :?> 'B) :> obj)
      )
    )

  let rep (pItem: Rule<'T, 'N>) : Rule<'T, 'N list> =
    let (Rule item) = pItem
    let nil: 'N list = []
    let term: Term<'T> = Term.Eps nil

    let t =
      Term.InfixLeft(term, item, term, (fun left mid _ -> ((mid :?> 'N) :: (left :?> 'N list)) |> box))

    Rule(Term.Map(t, (fun xs -> (xs :?> 'N list) |> List.rev |> box)))

  // build:

  let private doBuild<'T> (start: Symbol<'T>) (bindings: Binding<'T> list) : Parser<'T, obj> =
    // indexing
    let ruleArray = ResizeArray()
    let ruleRev = System.Collections.Generic.Dictionary()

    let dummy =
      Symbol(obj (), "dummy", lazy (Term.Choice []))

    ruleArray.Add(dummy)
    ruleRev.Add(dummy :> obj, 0)

    let ruleIdOf (rule: Symbol<'T>) =
      let (Symbol (id, _, _)) = rule
      id

    let indexOf (rule: Symbol<'T>) =
      let id = ruleIdOf rule

      if ruleRev.ContainsKey(id) |> not then
        failwithf "Rule not indexed (id:%A, rule:%A)" id rule

      ruleRev.[id]

    let intern (rule: Symbol<'T>) = ruleArray.[indexOf rule]

    (let internSymbol symbol =
      let (Symbol (id, _, _)) = symbol

      if ruleRev.ContainsKey(id) |> not then
        let index = ruleArray.Count
        ruleArray.Add(symbol)
        ruleRev.Add(id, index)

     let rec go (rule: Term<'T>) =
       match rule with
       | Term.Expect _
       | Term.Cut _
       | Term.Eps _ -> ()

       | Term.Symbol symbol -> internSymbol symbol

       | Term.Map (r, _) -> go r

       | Term.Seq (rules, _) ->
         for r in rules do
           go r

       | Term.Choice rules ->
         for r in rules do
           go r

       | Term.InfixLeft (rLeft, rMid, rRight, _) ->
         go rLeft
         go rMid
         go rRight

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

     let rec go nl (rule: Term<'T>) =
       match rule with
       | Term.Symbol symbol ->
         let (Symbol (_, name, _)) = symbol
         sprintf "R%d:%s" (indexOf symbol) name

       | Term.Expect (token, _) -> sprintf "expect(%A)" token
       | Term.Cut (token, _) -> sprintf "cut(%A)" token
       | Term.Eps _ -> "eps"
       | Term.Map (r, _) -> go nl r
       | Term.Seq (rules, _) -> sprintf "(%s)" (rules |> List.map (go nl) |> String.concat " ")

       | Term.Choice rules ->
         sprintf
           "(%s)"
           (rules
            |> List.map (go (nl + "  "))
            |> String.concat (nl + "| "))
       | Term.InfixLeft (rLeft, rMid, rRight, _) -> sprintf "(%s %%%s %s)" (go nl rLeft) (go nl rMid) (go nl rRight)

     for i in 1 .. ruleArray.Count - 1 do
       let rule = ruleArray.[i]

       let name, rule =
         let (Symbol (_, name, r)) = rule
         ":" + name, r.Value

       let initial = if hasChoice rule then "\n  " else " "
       eprintfn "R%d%s :=%s%s" i name initial (go "\n  " rule))

    ({ Start = start
       RuleArray = ruleArray.ToArray()
       RuleMemo = ruleRev
       Mapping = id }: Parser<_, _>)

  /// パーサを構築する
  ///
  /// - `start`: 開始規則
  /// - `bindings`: 再帰的な規則に定義をバインド(`bind`)するもの
  let build<'T, 'N> (start: Rule<'T, 'N>) (bindings: Binding<'T> list) : Parser<'T, 'N> =
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

    let p = doBuild start bindings

    ({ Start = p.Start
       RuleArray = p.RuleArray
       RuleMemo = p.RuleMemo
       Mapping = fun obj -> p.Mapping obj :?> 'N }: Parser<'T, 'N>)

  // parser methods:

  /// 構文規則を再帰的にたどって、バックトラックを利用してパースする
  let private parseNaively<'T, 'N> (getKind: 'T -> K) (tokens: 'T array) (parser: Parser<'T, 'N>) : 'N =
    let mutable index = 0
    let mutable cut = false

    let tokenAt i = tokens.[i]

    let shift cutting =
      assert (index < tokens.Length)
      let t = tokens.[index]
      eprintfn "shift %d:%A%s %A" index t (if cutting then "!" else "") t
      index <- index + 1

      if cutting then cut <- true

    let fail msg =
      let pos =
        if index < tokens.Length then
          sprintf "%d:%A" index (tokenAt index)
        else
          "EOF"

      failwithf "Parse Error: At %s, %s" pos msg

    let rec parseRec rule =
      match rule with
      | Term.Expect (token, extractor) ->
        if index < tokens.Length then
          let t = tokenAt index

          if getKind t = token then
            shift false
            extractor t
          else
            fail (sprintf "expect token '%A'" token)
        else
          fail (sprintf "expect token '%A'" token)

      | Term.Cut (token, extractor) ->
        if index < tokens.Length then
          let t = tokens.[index]

          if getKind t = token then
            shift true
            extractor t
          else
            fail (sprintf "expect token '%A'" token)
        else
          fail (sprintf "expect token '%A'" token)

      | Term.Symbol (Symbol (_, name, ruleLazy)) ->
        try
          parseRec ruleLazy.Value
        with
        | _ ->
          eprintfn "(While parsing %s at %d)" name index
          reraise ()

      | Term.Eps value -> value
      | Term.Map (r, mapping) -> parseRec r |> mapping
      | Term.Seq (rules, decode) -> rules |> List.map parseRec |> decode

      | Term.Choice rules ->
        let indexOrig = index
        let cutOrig = cut

        let rec chooseLoop rules =
          match rules with
          | [] -> fail "No alternative"

          | r :: rules ->
            try
              parseRec r
            with
            | _ ->
              if not cut then
                eprintfn "backtrack %d" index
                index <- indexOrig
                cut <- cutOrig
                chooseLoop rules
              else
                reraise ()

        cut <- false
        let result = chooseLoop rules
        cut <- cutOrig
        result

      | Term.InfixLeft (rLeft, rMid, rRight, decode) ->
        let rec infixLoop left =
          // try to parse mid
          let indexOrig = index
          let cutOrig = cut
          cut <- false

          let mid =
            try
              parseRec rMid |> Some
            with
            | _ ->
              // backtrack
              index <- indexOrig
              cut <- cutOrig
              None

          match mid with
          | Some mid ->
            let right = parseRec rRight
            infixLoop (decode left mid right)

          | None -> left

        let left = parseRec rLeft
        infixLoop left

    let r =
      let (Symbol (_, _, r)) = parser.Start
      r.Value

    parseRec r |> parser.Mapping

  /// トークン列をパースする
  let parseArray (getKind: 'T -> K) (tokens: 'T array) (parser: Parser<'T, 'N>) : 'N =
    parseNaively getKind tokens parser

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

  let private sParserLazy: Lazy<P.Parser<Token, Expr>> =
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
    P.parseArray TokenKind.ofToken tokenArray sParserLazy.Value

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

    assert (p
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

  let private sParserLazy: Lazy<P.Parser<Token, int list>> = lazy (P.build pSeq [])

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
    P.parseArray TokenKind.ofToken tokenArray sParserLazy.Value

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

let v1 () =
  Arith.tests ()
  NumberSequence.tests ()
