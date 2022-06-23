module rec Pcc.Program

open System.IO
open MyYacc

/// Parse tree element.
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type PElement =
  | Token of name: string * text: string
  | Node of name: string * children: PElement list

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Typ =
  | Int
  | Array of len: int
  | Name of id: string

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Dec =
  | Var of Typ * ids: string list
  | Type of id: string * Typ
  | Func of result: Typ * id: string * fargs: (Typ * string) list * Block
  | VoidFunc of id: string * fargs: (Typ * string) list * Block

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Block = { Decs: Dec list; Stmts: Stmt list }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Stmt =
  | Assign of id: string * value: Expr
  | IndexAssign of id: string * index: Expr * value: Expr
  | If of Cond * Stmt
  | IfElse of Cond * Stmt * Stmt
  | While of Cond * Stmt
  | SPrint of Expr
  | IPrint of Expr
  | Scan of id: string
  | New of id: string
  | CallProc of id: string * Expr list
  | Return of Expr
  | Block of Block
  | Nil

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Expr =
  | Num of value: int
  | Name of id: string
  | Call of id: string * Expr list
  | Index of id: string * Expr
  | Plus of Expr * Expr
  | Minus of Expr * Expr
  | Times of Expr * Expr
  | Div of Expr * Expr
  | UMinus of Expr
  | Paren of Expr

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Cond =
  | Eq of Expr * Expr
  | Neq of Expr * Expr
  | Gt of Expr * Expr
  | Lt of Expr * Expr
  | Ge of Expr * Expr
  | Le of Expr * Expr

let inline private unreachable context = failwithf "unreachable: %A" context

let private expectId token =
  match token with
  | PElement.Token ("ID", text) -> text
  | _ -> unreachable ()

let private expectNum token =
  match token with
  | PElement.Token ("NUM", text) -> int text
  | _ -> unreachable ()

let private castNode element =
  match element with
  | PElement.Node (name, children) -> name, children
  | _ -> unreachable ()

let private expectTy element =
  match castNode element with
  | "IntTyp", _ -> Typ.Int
  | "ArrayTyp", [ _; _; len; _ ] -> Typ.Array(expectNum len)
  | "NameTyp", [ id ] -> Typ.Name(expectId id)
  | _ -> unreachable element

let private expectDecs element =
  match element with
  | PElement.Node ("DecsNil", _) -> []
  | PElement.Node ("DecsCons", [ h; t ]) -> expectDec h :: expectDecs t
  | _ -> unreachable element

let private expectDec element =
  match castNode element with
  | "VarDec", [ ty; ids; _ ] -> Dec.Var(expectTy ty, expectIds ids)
  | "TypeDec", [ _; id; _; ty; _ ] -> Dec.Type(expectId id, expectTy ty)
  | "FuncDec", [ result; id; _; fargsOpt; _; block ] ->
    Dec.Func(expectTy result, expectId id, expectFargsOpt fargsOpt, expectBlock block)
  | "VoidFuncDec", [ _; id; fargsOpt; _; block ] ->
    Dec.VoidFunc(expectId id, expectFargsOpt fargsOpt, expectBlock block)
  | _ -> unreachable element

let private expectIds element =
  match element with
  | PElement.Node ("IdsSingle", [ id ]) -> [ expectId id ]
  | PElement.Node ("IdsCons", [ h; _; t ]) -> expectId h :: expectIds t
  | _ -> unreachable element

let private expectFargsOpt element =
  match element with
  | PElement.Node ("FArgsNone", _) -> []
  | PElement.Node ("FArgsSome", [ fargs ]) -> expectFargs fargs
  | _ -> unreachable element

let private expectFargs element =
  match element with
  | PElement.Node ("FArgsSingle", [ ty; id ]) -> [ expectTy ty, expectId id ]
  | PElement.Node ("FArgsCons", [ h; ty; id ]) -> List.append (expectFargs h) [ expectTy ty, expectId id ]
  | _ -> unreachable element

let private expectStmts element =
  match element with
  | PElement.Node ("StmtsCons", [ stmts; stmt ]) -> List.append (expectStmts stmts) [ expectStmt stmt ]
  | PElement.Node ("StmtsSingle", [ stmt ]) -> [ expectStmt stmt ]
  | _ -> unreachable element

let private expectStmt element =
  match castNode element with
  | "AssignStmt", [ id; _; rExpr; _ ] -> Stmt.Assign(expectId id, aExpr rExpr)
  | "IndexAssignStmt", [ id; _; index; _; rExpr; _ ] -> Stmt.IndexAssign(expectId id, aExpr index, aExpr rExpr)
  | "IfStmt", [ _; _; cond; _; body ] -> Stmt.If(expectCond cond, expectStmt body)
  | "IfElseStmt", [ _; _; cond; _; body; _; alt ] -> Stmt.IfElse(expectCond cond, expectStmt body, expectStmt alt)
  | "WhileStmt", [ _; _; cond; _; body ] -> Stmt.While(expectCond cond, expectStmt body)
  | "SPrintStmt", [ _; _; arg; _; _ ] -> Stmt.SPrint(aExpr arg)
  | "IPrintStmt", [ _; _; arg; _; _ ] -> Stmt.IPrint(aExpr arg)
  | "ScanStmt", [ _; _; id; _; _ ] -> Stmt.Scan(expectId id)
  | "NewStmt", [ _; _; id; _; _ ] -> Stmt.New(expectId id)
  | "CallProcStmt", [ id; _; aargsOpt; _; _ ] -> Stmt.CallProc(expectId id, expectAargsOpt aargsOpt)
  | "ReturnStmt", [ _; expr; _ ] -> Stmt.Return(aExpr expr)
  | "BlockStmt", [ block ] -> Stmt.Block(expectBlock block)
  | "NilStmt", _ -> Stmt.Nil
  | _ -> unreachable element

let private expectAargs element =
  match element with
  | PElement.Node ("AArgsSingle", [ expr ]) -> [ aExpr expr ]
  | PElement.Node ("AArgsCons", [ h; t ]) -> aExpr h :: expectAargs t
  | _ -> unreachable ()

let private expectAargsOpt element =
  match element with
  | PElement.Node ("AArgsNone", _) -> []
  | PElement.Node ("AArgsSome", [ aargs ]) -> expectAargs aargs
  | _ -> unreachable ()

let private expectBlock element : Block =
  match element with
  | PElement.Node ("Block", [ _; decs; stmts; _ ]) ->
    { Decs = expectDecs decs
      Stmts = expectStmts stmts }
  | _ -> unreachable element

let private aExpr element : Expr =
  let onBinary makeNode =
    match castNode element with
    | _, [ lExpr; _; rExpr ] -> makeNode (aExpr lExpr, aExpr rExpr)
    | _ -> unreachable ()

  match castNode element with
  | "NumExpr", [ token ] -> Expr.Num(expectNum token)
  | "NameExpr", [ token ] -> Expr.Name(expectId token)
  | "CallExpr", [ id; _; aargs; _ ] -> Expr.Call(expectId id, expectAargs aargs)
  | "IndexExpr", [ id; _; arg; _ ] -> Expr.Index(expectId id, aExpr arg)
  | "PlusExpr", _ -> onBinary Expr.Plus
  | "MinusExpr", _ -> onBinary Expr.Minus
  | "TimesExpr", _ -> onBinary Expr.Times
  | "DivExpr", _ -> onBinary Expr.Div
  | "UMinusExpr", [ _; arg ] -> Expr.UMinus(aExpr arg)
  | "ParenExpr", [ _; body; _ ] -> Expr.Paren(aExpr body)
  | _ -> unreachable element

let rec expectCond element =
  let name, children = castNode element

  let makeNode =
    match name with
    | "EqCond" -> Cond.Eq
    | "NeqCond" -> Cond.Neq
    | "GtCond" -> Cond.Gt
    | "LtCond" -> Cond.Lt
    | "GeCond" -> Cond.Ge
    | "LeCond" -> Cond.Le
    | _ -> unreachable element

  match children with
  | [ lExpr; _; rExpr ] -> makeNode (aExpr lExpr, aExpr rExpr)
  | _ -> unreachable element

let private expectRoot element =
  match element with
  | PElement.Node ("Root", [ stmt ]) -> expectStmt stmt
  | _ -> unreachable element

[<EntryPoint>]
let main _ =
  let args = System.Environment.GetCommandLineArgs()
  let lexText = File.ReadAllText(args.[1])
  let grammarText = File.ReadAllText(args.[2])
  let input = stdin.ReadToEnd()

  // Generate lexer.
  let rules =
    try
      MyLex.parseLexer lexText
    with
    | MyLex.ParseLexerException (msg, row, column) ->
      printfn "FATAL: Invalid lexer. %s at %d:%d" msg (row + 1) (column + 1)
      exit 1

  let nfa = MyLex.generateNfa rules

  // Generate parser.
  let parser =
    try
      MyYacc.generateLrParser grammarText
    with
    | MyYacc.ParseGrammarException (msg, i) ->
      eprintfn "FATAL: Invalid grammar. %s at %d" msg i
      exit 1

  // let dumpGrammar () = MyYacc.dump grammarText

  // Tokenize.
  let tokens =
    try
      MyLex.tokenizeWithNfa input nfa
    with
    | MyLex.TokenizeException index ->
      printfn "ERROR: Tokenize failed at %d\n" index
      printfn "  %s" input
      printfn "  %s^" (String.replicate index " ")
      exit 1

  let tokens =
    let t = ResizeArray()
    let mutable cursor = 0

    for kind, len in tokens do
      if kind <> "SPACE" then
        t.Add(kind, input.[cursor .. cursor + len - 1])

      cursor <- cursor + len

    t.ToArray()

  // let dumpTokens () =
  //   tokens
  //   |> List.map (fun (kind, len) -> sprintf "%s(%d)" kind len)
  //   |> String.concat " "
  //   |> printfn "  %s"

  // Parse.
  let events =
    let tokens = tokens |> Array.map fst |> Array.toList
    MyYacc.LrParser.parse tokens parser

  let dumpParseEvents events =
    let rec go indent count events =
      match events with
      | _ when count = 0 -> events

      | MyYacc.ParseEvent.Token i :: events ->
        eprintfn "%s%s %s" indent "token" (fst tokens.[i])
        go indent (count - 1) events

      | MyYacc.ParseEvent.StartNode (name, childrenCount) :: events ->
        eprintfn "%s%s %s (%d)" indent "node" name childrenCount
        let events = go (indent + "  ") childrenCount events
        go indent (count - 1) events

      | [] -> failwith "unreachable"

    let events = go "" 1 events
    eprintfn "rest: %A" events

  let root =
    let rec go acc count events =
      match events with
      | _ when count = 0 -> List.rev acc, events

      | MyYacc.ParseEvent.Token i :: events ->
        let kind, text = tokens.[i]
        go (PElement.Token(kind, text) :: acc) (count - 1) events

      | MyYacc.ParseEvent.StartNode (name, childrenCount) :: events ->
        let children, events = go [] childrenCount events
        go (PElement.Node(name, children) :: acc) (count - 1) events

      | [] -> failwith "unreachable"

    let children, events = go [] 1 events

    if List.isEmpty events |> not then
      eprintfn "rest = %A" events
      assert false

    children
    |> List.tryExactlyOne
    |> Option.defaultWith unreachable

  let ast = expectRoot root
  eprintfn "%A" ast
  0
