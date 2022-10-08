module Program

type private HashSet<'T> = System.Collections.Generic.HashSet<'T>
type private HashMap<'K, 'T> = System.Collections.Generic.Dictionary<'K, 'T>

let inline private todo () = failwith "todo"
let inline private unreachable () = failwith "unreachable"
let inline private (|Unreachable|) _ = unreachable ()

module private ParserCombinator =
  [<RequireQualifiedAccess>]
  type private Shape<'T> =
    | Expect of 'T
    | Cut of 'T
    | Ref of id: obj
    | Box of id: obj
    | Seq of Shape<'T> list
    | Choice of Shape<'T> list

  /// Kind of token.
  type private K = int

  [<RequireQualifiedAccess>]
  type private Term<'T> =
    private
    | Expect of K * extractor: ('T -> obj)
    | Cut of K * extractor: ('T -> obj)

    | Symbol of Symbol<'T>
    | Map of Term<'T> * mapping: (obj -> obj)
    | Seq of Term<'T> list * decode: (obj list -> obj)
    | Choice of Term<'T> list
    | InfixLeft of Term<'T> * Term<'T> * Term<'T> * (obj -> obj -> obj -> obj)

  and private Symbol<'T> = Symbol of id: obj * name: string * Lazy<Term<'T>>

  type Rule<'T, 'N> = private Rule of Term<'T>

  type RecRule<'T, 'N> = private RecRule of name: string * rule: Rule<'T, 'N> option ref

  type Binding<'T> = private Binding of Term<'T>

  type Parser<'T, 'N> =
    private
      { Start: Symbol<'T>
        RuleArray: Symbol<'T> array
        RuleMemo: HashMap<obj, int>
        Mapping: (obj -> 'N) }

  // tokens:

  let look (token: int) : Rule<'T, unit> = todo ()

  let expect (token: int) (extractor: 'T -> 'N) : Rule<'T, 'N> =
    Rule(Term.Expect(token, extractor >> box))

  let cut (token: int) (extractor: 'T -> 'N) : Rule<'T, 'N> = Rule(Term.Cut(token, extractor >> box))

  // nominal rules:

  let recursive (name: string) : RecRule<_, _> = RecRule(name, ref None)

  let recurse (rule: RecRule<'T, 'N>) : Rule<'T, 'N> =
    let (RecRule (name, ruleRef)) = rule

    Symbol(
      ruleRef :> obj,
      name,
      lazy
        (match ruleRef.contents with
         | Some (Rule it) -> it
         | None -> failwithf "Rule '%s' unbound" name)
    )
    |> Term.Symbol
    |> Rule

  let bind (recRule: RecRule<'T, 'N>) (actualRule: Rule<'T, 'N>) : Binding<'T> =
    let (RecRule (_, ruleRef)) = recRule
    ruleRef.contents <- Some actualRule

    let (Rule r) = actualRule
    Binding r

  let label (name: string) (rule: Rule<'T, 'N>) : Rule<'T, 'N> = todo ()

  // algebraic rules:

  let eps () : Rule<'T, 'N> = todo ()

  let rule1 (r: Rule<'T, 'A>) (mapping: 'A -> 'N) : Rule<'T, 'N> =
    let (Rule r) = r

    Term.Map(r, (fun obj -> mapping (obj :?> 'A) :> obj)) |> Rule

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

  let choice (rules: Rule<'T, 'N> list) : Rule<'T, 'N> =
    Rule(Term.Choice(List.map (fun (Rule r) -> r) rules))

  let infixLeft
    (pLeft: Rule<'T, 'N>)
    (pMid: Rule<'T, 'A>)
    (pRight: Rule<'T, 'B>)
    (decode: 'N -> 'A -> 'B -> 'N)
    : Rule<'T, 'N> =
    let (Rule rLeft) = pLeft
    let (Rule rMid) = pMid
    let (Rule rRight) = pRight

    Rule(Term.InfixLeft(rLeft, rMid, rRight, (fun left mid right -> decode (left :?> 'N) (mid :?> 'A) (right :?> 'B) :> obj)))

  // build:

  let private doBuild<'T> (start: Symbol<'T>) (bindings: Binding<'T> list) : Parser<'T, obj> =
    // indexing
    let ruleArray = ResizeArray()
    let ruleRev = System.Collections.Generic.Dictionary()

    let dummy = Symbol(obj (), "dummy", lazy (Term.Choice []))

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

    (let rec go (rule: Term<'T>) =
      match rule with
      | Term.Expect _
      | Term.Cut _ -> ()

      | Term.Symbol ((Symbol (id, _, _)) as symbol) ->
        if ruleRev.ContainsKey(id) |> not then
          let index = ruleArray.Count
          ruleArray.Add(symbol)
          ruleRev.Add(id, index)

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

     for Binding rule in bindings do
       go rule)

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
       | Term.Map (r, _) -> go nl r
       | Term.Seq (rules, _) -> sprintf "(%s)" (rules |> List.map (go nl) |> String.concat " ")

       | Term.Choice rules -> sprintf "(%s)" (rules |> List.map (go (nl + "  ")) |> String.concat (nl + "| "))
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

  let private interpret<'T, 'N> (getKind: 'T -> K) (tokens: 'T array) (parser: Parser<'T, 'N>) : 'N =
    let mutable index = 0
    let mutable cut = false

    let tokenAt i = tokens.[i]

    let shift cutting =
      assert (index < tokens.Length)
      let t = tokens.[index]
      eprintfn "shift %d:%A%s %A" index t (if cutting then "!" else "") t
      index <- index + 1

      if cutting then
        cut <- true

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
        with _ ->
          eprintfn "(While parsing %s at %d)" name index
          reraise ()

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
            with _ ->
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
            with _ ->
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

  let parseArray (getKind: 'T -> K) (tokens: 'T array) (parser: Parser<'T, 'N>) : 'N = interpret getKind tokens parser

module private Arith1 =
  module P = ParserCombinator

  [<RequireQualifiedAccess>]
  type Token =
    | Number
    | Plus

  [<RequireQualifiedAccess>]
  type Expr =
    | Number of int
    | Add of Expr * Expr

module private Arith =
  module P = ParserCombinator

  [<RequireQualifiedAccess>]
  type internal Token =
    | Number of int
    | Plus
    | Minus
    | Star
    | Slash
    | LeftParen
    | RightParen

  module private TokenKind =
    let Number = 1
    let Plus = 2
    let Minus = 3
    let Star = 4
    let Slash = 5
    let LeftParen = 6
    let RightParen = 7

  [<RequireQualifiedAccess>]
  type internal Binary =
    | Add
    | Subtract
    | Multiply
    | Divide

  [<RequireQualifiedAccess>]
  type internal Expr =
    | Number of int
    | Paren of Expr
    | BinOp of Binary * Expr * Expr

  let private pExpr: P.RecRule<_, Expr> = P.recursive "Expression"

  let private pParen =
    P.rule3 (P.cut TokenKind.LeftParen ignore) (P.recurse pExpr) (P.expect TokenKind.RightParen ignore) (fun _ e _ ->
      Expr.Paren e)

  let private pPrimary =
    P.choice
      [ P.cut TokenKind.Number (fun (Token.Number value | Unreachable value) -> Expr.Number value)
        pParen ]

  let private pMul =
    P.infixLeft
      pPrimary
      (P.choice
        [ P.cut TokenKind.Star (fun _ -> Binary.Multiply)
          P.cut TokenKind.Slash (fun _ -> Binary.Divide) ])
      pPrimary
      (fun l op r -> Expr.BinOp(op, l, r))

  let private pAdd =
    P.infixLeft
      pMul
      (P.choice
        [ P.cut TokenKind.Plus (fun _ -> Binary.Add)
          P.cut TokenKind.Minus (fun _ -> Binary.Subtract) ])
      pMul
      (fun l op r -> Expr.BinOp(op, l, r))

  let private sParser: Lazy<P.Parser<Token, _>> =
    lazy (P.build (P.recurse pExpr) [ P.bind pExpr pAdd ])

  let private tokenize (text: string) : Token array =
    text.Replace("(", "( ").Replace(")", " )").Split(" ")
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

  let private getKind token =
    match token with
    | Token.Number _ -> TokenKind.Number
    | Token.Plus -> TokenKind.Plus
    | Token.Minus -> TokenKind.Minus
    | Token.Star -> TokenKind.Star
    | Token.Slash -> TokenKind.Slash
    | Token.LeftParen -> TokenKind.LeftParen
    | Token.RightParen -> TokenKind.RightParen

  let internal parseString (s: string) =
    P.parseArray getKind (tokenize s) sParser.Value

  let internal tests () =
    let p s x =
      let actual = parseString s |> sprintf "%A"

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
    assert (p "2 * 3 / 5 * 7" "BinOp\n  (Multiply, BinOp (Divide, BinOp (Multiply, Number 2, Number 3), Number 5),\n   Number 7)")

[<EntryPoint>]
let main _ =
  Arith.tests ()
  0
