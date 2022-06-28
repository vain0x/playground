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
  | PElement.Node ("DecsCons", [ h; t ]) -> lowerDec h :: lowerDecs t
  | _ -> unreachable element

let private lowerDec element =
  match unwrapNode element with
  | "VarDec", [ ty; ids; _ ] -> Dec.Var(lowerTyp ty, lowerIds ids)
  | "TypeDec", [ _; id; _; ty; _ ] -> Dec.Type(lowerId id, lowerTyp ty)
  | "FuncDec", [ result; id; _; fargsOpt; _; block ] ->
    Dec.Func(lowerTyp result, lowerId id, lowerFargsOpt fargsOpt, lowerBlock block)
  | "VoidFuncDec", [ _; id; fargsOpt; _; block ] -> Dec.VoidFunc(lowerId id, lowerFargsOpt fargsOpt, lowerBlock block)
  | _ -> unreachable element

let private lowerIds element =
  match element with
  | PElement.Node ("IdsSingle", [ id ]) -> [ lowerId id ]
  | PElement.Node ("IdsCons", [ h; _; t ]) -> lowerId h :: lowerIds t
  | _ -> unreachable element

let private lowerFargsOpt element =
  match element with
  | PElement.Node ("FArgsNone", _) -> []
  | PElement.Node ("FArgsSome", [ fargs ]) -> lowerFargs fargs
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
  | "AssignStmt", [ id; _; rExpr; _ ] -> Stmt.Assign(lowerId id, lowerExpr rExpr)
  | "IndexAssignStmt", [ id; _; index; _; rExpr; _ ] -> Stmt.IndexAssign(lowerId id, lowerExpr index, lowerExpr rExpr)
  | "IfStmt", [ _; _; cond; _; body ] -> Stmt.If(lowerCond cond, lowerStmt body)
  | "IfElseStmt", [ _; _; cond; _; body; _; alt ] -> Stmt.IfElse(lowerCond cond, lowerStmt body, lowerStmt alt)
  | "WhileStmt", [ _; _; cond; _; body ] -> Stmt.While(lowerCond cond, lowerStmt body)
  | "SPrintStmt", [ _; _; arg; _; _ ] -> Stmt.SPrint(lowerExpr arg)
  | "IPrintStmt", [ _; _; arg; _; _ ] -> Stmt.IPrint(lowerExpr arg)
  | "ScanStmt", [ _; _; id; _; _ ] -> Stmt.Scan(lowerId id)
  | "NewStmt", [ _; _; id; _; _ ] -> Stmt.New(lowerId id)
  | "CallProcStmt", [ id; _; aargsOpt; _; _ ] -> Stmt.CallProc(lowerId id, lowerAargsOpt aargsOpt)
  | "ReturnStmt", [ _; expr; _ ] -> Stmt.Return(lowerExpr expr)
  | "BlockStmt", [ block ] -> Stmt.Block(lowerBlock block)
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

let private lowerBlock element : Block =
  match element with
  | PElement.Node ("Block", [ _; decs; stmts; _ ]) ->
    { Decs = lowerDecs decs
      Stmts = lowerStmts stmts }
  | _ -> unreachable element

let private lowerExpr element : Expr =
  let onBinary makeNode =
    match unwrapNode element with
    | _, [ lExpr; _; rExpr ] -> makeNode (lowerExpr lExpr, lowerExpr rExpr)
    | _ -> unreachable ()

  match unwrapNode element with
  | "NumExpr", [ token ] -> Expr.Num(lowerNum token)
  | "NameExpr", [ token ] -> Expr.Name(lowerId token)
  | "CallExpr", [ id; _; aargs; _ ] -> Expr.Call(lowerId id, lowerAargs aargs)
  | "IndexExpr", [ id; _; arg; _ ] -> Expr.Index(lowerId id, lowerExpr arg)
  | "PlusExpr", _ -> onBinary Expr.Plus
  | "MinusExpr", _ -> onBinary Expr.Minus
  | "TimesExpr", _ -> onBinary Expr.Times
  | "DivExpr", _ -> onBinary Expr.Div
  | "UMinusExpr", [ _; arg ] -> Expr.UMinus(lowerExpr arg)
  | "ParenExpr", [ _; body; _ ] -> Expr.Paren(lowerExpr body)
  | _ -> unreachable element

let private lowerCond element =
  let name, children = unwrapNode element

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
  | [ lExpr; _; rExpr ] -> makeNode (lowerExpr lExpr, lowerExpr rExpr)
  | _ -> unreachable element

let lowerRoot element =
  match element with
  | PElement.Node ("Root", [ stmt ]) -> lowerStmt stmt
  | _ -> unreachable element
