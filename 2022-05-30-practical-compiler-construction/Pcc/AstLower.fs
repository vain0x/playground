module rec Pcc.AstLower

// Lower parse tree to AST.

open MyYacc
open Pcc.Ast

let inline private unreachable context = failwithf "unreachable: %A" context

let private lowerId token =
  match token with
  | PElement.Token ("ID", text) -> text
  | _ -> unreachable ()

let private lowerNum token =
  match token with
  | PElement.Token ("NUM", text) -> int text
  | _ -> unreachable ()

let private unwrapNode element =
  match element with
  | PElement.Node (name, children) -> name, children
  | _ -> unreachable ()

let private lowerTyp element =
  match unwrapNode element with
  | "IntTyp", _ -> Typ.Int
  | "ArrayTyp", [ _; _; len; _ ] -> Typ.Array(lowerNum len)
  | "NameTyp", [ id ] -> Typ.Name(lowerId id)
  | _ -> unreachable element

let private lowerDecs element =
  match element with
  | PElement.Node ("DecsNil", _) -> []

  // FIXME: `decs` が空列に還元されたとき空の宣言列が子要素として追加されない
  //        (空許容な非終端記号の飛び越しを同一の状態に含める場合)
  // | PElement.Node ("DecsCons", [ t ]) -> [ lowerDec t ]

  | PElement.Node ("DecsCons", [ h; t ]) -> List.append (lowerDecs h) [ lowerDec t ]

  // FIXME: パーサの実装不備により空列はEpsという名前のノードになってしまう (DecsNilであるべき)
  | PElement.Node ("Eps", _) -> []

  | _ -> unreachable element

let private lowerDec element =
  match unwrapNode element with
  | "VarDec", [ ty; ids; _ ] ->
    lowerIds ids
    |> List.map (fun name -> Dec.Var(lowerTyp ty, name))
  | "TypeDec", [ _; id; _; ty; _ ] ->
    Dec.Type(lowerId id, lowerTyp ty)
    |> List.singleton
  | "FuncDec", [ result; id; _; fargsOpt; _; block ] ->
    Dec.Func(lowerId id, lowerFargsOpt fargsOpt, lowerTyp result, lowerBlock block)
    |> List.singleton
  | "VoidFuncDec", [ _; id; fargsOpt; _; block ] ->
    Dec.Func(lowerId id, lowerFargsOpt fargsOpt, Typ.Void, lowerBlock block)
    |> List.singleton
  | _ -> unreachable element

let private lowerIds element : string list =
  match element with
  | PElement.Node ("IdsSingle", [ id ]) -> [ lowerId id ]
  | PElement.Node ("IdsCons", [ h; _; t ]) -> List.append (lowerIds h) [ lowerId t ]
  | _ -> unreachable element

let private lowerFargsOpt element =
  match element with
  | PElement.Node ("FArgsOptNone", _) -> []
  | PElement.Node ("FArgsOptSome", [ fargs ]) -> lowerFargs fargs

  // FIXME: パーサの実装不備
  | PElement.Node ("Eps", _) -> []

  | _ -> unreachable element

let private lowerFargs element =
  match element with
  | PElement.Node ("FArgsSingle", [ ty; id ]) -> [ lowerTyp ty, lowerId id ]
  | PElement.Node ("FArgsCons", [ h; ty; id ]) -> List.append (lowerFargs h) [ lowerTyp ty, lowerId id ]
  | _ -> unreachable element

let private lowerStmts element =
  match element with
  | PElement.Node ("StmtsCons", [ stmts; stmt ]) -> List.append (lowerStmts stmts) [ lowerStmt stmt ]
  | PElement.Node ("StmtsSingle", [ stmt ]) -> [ lowerStmt stmt ]
  | _ -> unreachable element

let private lowerStmt element =
  match unwrapNode element with
  | "AssignStmt", [ id; _; rExpr; _ ] -> Stmt.Assign(Var(lowerId id), lowerExpr rExpr)
  | "IndexAssignStmt", [ id; _; index; _; rExpr; _ ] ->
    Stmt.Assign(IndexedVar(lowerId id, lowerExpr index), lowerExpr rExpr)
  | "IfStmt", [ _; _; cond; _; body ] -> Stmt.If(lowerCond cond, lowerStmt body, None)
  | "IfElseStmt", [ _; _; cond; _; body; _; alt ] -> Stmt.If(lowerCond cond, lowerStmt body, Some(lowerStmt alt))
  | "WhileStmt", [ _; _; cond; _; body ] -> Stmt.While(lowerCond cond, lowerStmt body)
  | "SPrintStmt", [ _; _; arg; _; _ ] -> Stmt.CallProc("sprint", [ lowerExpr arg ])
  | "IPrintStmt", [ _; _; arg; _; _ ] -> Stmt.CallProc("iprint", [ lowerExpr arg ])
  | "ScanStmt", [ _; _; id; _; _ ] -> Stmt.CallProc("scan", [ Expr.Var(Var(lowerId id)) ])
  | "NewStmt", [ _; _; id; _; _ ] -> Stmt.CallProc("new", [ Expr.Var(Var(lowerId id)) ])
  | "CallProcStmt", [ id; _; aargsOpt; _; _ ] -> Stmt.CallProc(lowerId id, lowerAargsOpt aargsOpt)
  | "ReturnStmt", [ _; expr; _ ] -> Stmt.CallProc("return", [ lowerExpr expr ])
  | "BlockStmt", [ block ] -> lowerBlock block
  | "NilStmt", _ -> Stmt.Nil
  | _ -> unreachable element

let private lowerAargs element =
  match element with
  | PElement.Node ("AArgsSingle", [ expr ]) -> [ lowerExpr expr ]
  | PElement.Node ("AArgsCons", [ h; t ]) -> lowerExpr h :: lowerAargs t
  | _ -> unreachable ()

let private lowerAargsOpt element =
  match element with
  | PElement.Node ("AArgsNone", _) -> []
  | PElement.Node ("AArgsSome", [ aargs ]) -> lowerAargs aargs
  | _ -> unreachable ()

let private lowerBlock element =
  match element with
  | PElement.Node ("Block", [ _; decs; stmts; _ ]) -> Stmt.Block(List.collect id (lowerDecs decs), lowerStmts stmts)
  | _ -> unreachable element

let private lowerExpr element : Expr =
  let onBinary name =
    match unwrapNode element with
    | _, [ lExpr; _; rExpr ] -> Expr.Call(name, [ lowerExpr lExpr; lowerExpr rExpr ])
    | _ -> unreachable ()

  match unwrapNode element with
  | "NumExpr", [ num ] -> Expr.Num(lowerNum num)
  | "NameExpr", [ id ] -> Expr.Var(Var(lowerId id))
  | "CallExpr", [ id; _; aargs; _ ] -> Expr.Call(lowerId id, lowerAargs aargs)
  | "IndexExpr", [ id; _; arg; _ ] -> Expr.Var(IndexedVar(lowerId id, lowerExpr arg))
  | "PlusExpr", _ -> onBinary "+"
  | "MinusExpr", _ -> onBinary "-"
  | "TimesExpr", _ -> onBinary "*"
  | "DivExpr", _ -> onBinary "/"
  | "UMinusExpr", [ _; arg ] -> Expr.Call("!", [ lowerExpr arg ])
  | "ParenExpr", [ _; body; _ ] -> lowerExpr body
  | _ -> unreachable element

let private lowerCond element =
  let name, children = unwrapNode element

  let makeNode =
    match name with
    | "EqCond" -> "=="
    | "NeqCond" -> "!="
    | "GtCond" -> ">"
    | "LtCond" -> "<"
    | "GeCond" -> ">="
    | "LeCond" -> "<="
    | _ -> unreachable element

  match children with
  | [ lExpr; _; rExpr ] -> Expr.Call(name, [ lowerExpr lExpr; lowerExpr rExpr ])
  | _ -> unreachable element

let lowerRoot element =
  match element with
  | PElement.Node ("Root", [ stmt ]) -> lowerStmt stmt
  | _ -> unreachable element
