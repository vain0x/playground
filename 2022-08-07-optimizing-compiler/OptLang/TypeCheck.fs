module rec OptLang.TypeCheck

open OptLang.Symbol
open OptLang.Tir

module S = OptLang.Syntax

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private ValueDef =
  | Constant of TExpr
  | Local of Symbol * TTy
  | Fn of Symbol * TTy list * TTy
  | Unary of TUnary
  | Binary of TBinary
  | LogAnd
  | LogOr
  | ArrayPush
  | Assert

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private FnDef = { ParamTys: TTy list; ResultTy: TTy }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private RecordTyDef = { Fields: (Symbol * TTy) array }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private TcState =
  { mutable ValueEnv: Map<string, ValueDef>
    mutable TyEnv: Map<string, TTy>
    Locals: ResizeArray<Symbol * TTy>
    Fns: ResizeArray<FnDef>
    Records: ResizeArray<RecordTyDef> }

let private builtInValueEnv =
  [ "void", ValueDef.Constant TExpr.Void
    "false", ValueDef.Constant(TExpr.Bool false)
    "true", ValueDef.Constant(TExpr.Bool true)
    "not", ValueDef.Unary TUnary.Not
    "minus", ValueDef.Unary TUnary.Minus

    "||", ValueDef.LogOr
    "&&", ValueDef.LogAnd
    "==", ValueDef.Binary TBinary.Equal
    "!=", ValueDef.Binary TBinary.NotEqual
    "<", ValueDef.Binary TBinary.LessThan
    "<=", ValueDef.Binary TBinary.LessEqual
    ">", ValueDef.Binary TBinary.GreaterThan
    ">=", ValueDef.Binary TBinary.GreaterEqual
    "+", ValueDef.Binary TBinary.Add
    "-", ValueDef.Binary TBinary.Subtract
    "*", ValueDef.Binary TBinary.Multiply
    "/", ValueDef.Binary TBinary.Divide
    "%", ValueDef.Binary TBinary.Modulo

    "push", ValueDef.ArrayPush
    "assert", ValueDef.Assert ]
  |> Map.ofList

let private initialState () : TcState =
  { ValueEnv = builtInValueEnv
    TyEnv = Map.empty
    Locals = ResizeArray()
    Fns = ResizeArray()
    Records = ResizeArray() }

let private cloneState (state: TcState) : TcState =
  { state with
      ValueEnv = state.ValueEnv
      TyEnv = state.TyEnv
      Locals = state.Locals }

let private isComparison (binary: TBinary) =
  match binary with
  | TBinary.Equal
  | TBinary.NotEqual
  | TBinary.LessThan
  | TBinary.LessEqual
  | TBinary.GreaterThan
  | TBinary.GreaterEqual -> true
  | _ -> false

let private unreachable () = failwith "unreachable"

let inline private withContext (label: string) (data: obj) ([<InlineIfLambda>] action) =
  try
    action ()
  with
  | _ ->
    eprintfn "In %s: %A" label data
    reraise ()

let private mismatch actual expected =
  failwithf "Type mismatch. Expected %s but was %s" expected actual

let private unifyTy (actualTy: TTy) (expectedTy: TTy) =
  if actualTy <> expectedTy then
    failwithf "Type mismatch. Expected %A but was %A" expectedTy actualTy

let private placeToTy (state: TcState) (place: TPlace) : TTy =
  match place with
  | TPlace.Local symbol ->
    let _, ty = state.Locals.[symbol.Index]
    ty

  | TPlace.Index (lhs, _) ->
    match placeToTy state lhs with
    | TTy.Array itemTy -> itemTy
    | _ -> unreachable ()

  | TPlace.Field (lhs, field) ->
    match placeToTy state lhs with
    | TTy.Record record ->
      state.Records.[record.Index].Fields.[field.Index]
      |> snd

    | _ -> unreachable ()

let private exprToTy (state: TcState) (expr: TExpr) : TTy =
  match expr with
  | TExpr.Void -> TTy.Void
  | TExpr.Bool _ -> TTy.Bool
  | TExpr.Int _ -> TTy.Int
  | TExpr.String _ -> TTy.String
  | TExpr.Read place -> placeToTy state place

  | TExpr.Array (itemTy, _) -> TTy.Array itemTy
  | TExpr.Record (record, _) -> TTy.Record record

  | TExpr.Unary (unary, _) ->
    match unary with
    | TUnary.Not -> TTy.Bool

    | TUnary.Minus
    | TUnary.ArrayLen -> TTy.Int

  | TExpr.Binary (binary, _, _) ->
    match binary with
    | TBinary.Add
    | TBinary.Subtract
    | TBinary.Multiply
    | TBinary.Divide
    | TBinary.Modulo -> TTy.Int

    | TBinary.Equal
    | TBinary.NotEqual
    | TBinary.LessThan
    | TBinary.LessEqual
    | TBinary.GreaterThan
    | TBinary.GreaterEqual -> TTy.Bool

  | TExpr.Call (callable, _) ->
    match callable with
    | TCallable.Fn fn -> state.Fns.[fn.Index].ResultTy

    | TCallable.LogOr
    | TCallable.LogAnd -> TTy.Bool

    | TCallable.ArrayPush
    | TCallable.Assert -> TTy.Void

let private checkTy (state: TcState) (ty: S.Ty) : TTy =
  match ty with
  | S.Ty.Void -> TTy.Void
  | S.Ty.Bool -> TTy.Bool
  | S.Ty.Int -> TTy.Int
  | S.Ty.String -> TTy.String

  | S.Ty.Name name ->
    match state.TyEnv |> Map.tryFind name with
    | Some ty -> ty
    | None -> failwithf "Undefined type '%s'" name

  | S.Ty.Array itemTy -> TTy.Array(checkTy state itemTy)

let private synthFieldAsPlace (state: TcState) expr : Result<TPlace, TExpr> =
  let lhs, field =
    match expr with
    | S.Expr.Field (lhs, field) -> lhs, field
    | _ -> unreachable ()

  let lhs = synthAsPlace state lhs

  match placeToTy state lhs with
  | TTy.Record record ->
    let fields = state.Records.[record.Index].Fields

    match fields
          |> Array.tryPick (fun ((f: Symbol), _) -> if f.Name = field then Some f else None)
      with
    | Some field -> Ok(TPlace.Field(lhs, field))
    | None -> failwithf "Missing field '%s' in a record type '%A'" field record

  | TTy.Array _ ->
    match field with
    | "len" -> Error(TExpr.Unary(TUnary.ArrayLen, TExpr.Read lhs))
    | _ -> failwithf "Array doesn't have field '%s'" field

  | _ -> mismatch (string (placeToTy state lhs)) "record"

let private synthAsPlace (state: TcState) (expr: S.Expr) : TPlace =
  match expr with
  | S.Expr.Name name ->
    match state.ValueEnv |> Map.tryFind name with
    | Some (ValueDef.Local (local, _)) -> TPlace.Local local
    | Some _ -> failwithf "Function can't be used as a value '%s'" name
    | None -> failwithf "Undefined variable '%s'" name

  | S.Expr.Index (lhs, index) ->
    let lhs = synthAsPlace state lhs

    match placeToTy state lhs with
    | TTy.Array _ -> ()
    | ty -> mismatch (string ty) "array(_)"

    let index = checkExpr state index TTy.Int
    TPlace.Index(lhs, index)

  | S.Expr.Field _ ->
    match synthFieldAsPlace state expr with
    | Ok it -> it
    | Error _ -> failwithf "Expected a place but was %A" expr

  | _ -> failwithf "Expected a place but was %A" expr

let private checkAsPlace (state: TcState) (expr: S.Expr) (ty: TTy) : TPlace =
  match expr with
  | S.Expr.Index (lhs, index) ->
    let lhs = checkAsPlace state lhs (TTy.Array ty)
    let index = checkExpr state index TTy.Int
    TPlace.Index(lhs, index)

  | S.Expr.Name _
  | S.Expr.Field _ ->
    let place = synthAsPlace state expr
    unifyTy (placeToTy state place) ty
    place

  | _ -> failwithf "Expected a place but was %A" expr

let private synthExpr (state: TcState) (expr: S.Expr) : TExpr =
  match expr with
  | S.Expr.Name name ->
    match state.ValueEnv |> Map.tryFind name with
    | Some (ValueDef.Constant expr) -> expr
    | _ ->
      let place = synthAsPlace state expr
      TExpr.Read place

  | S.Expr.Int value -> TExpr.Int value
  | S.Expr.String value -> TExpr.String value

  | S.Expr.Array (head :: tail) ->
    let head = synthExpr state head
    let itemTy = exprToTy state head

    let tail =
      tail
      |> List.map (fun item -> checkExpr state item itemTy)

    TExpr.Array(itemTy, head :: tail)

  | S.Expr.Index _ -> TExpr.Read(synthAsPlace state expr)

  | S.Expr.Field _ ->
    match synthFieldAsPlace state expr with
    | Ok place -> TExpr.Read place
    | Error expr -> expr

  | S.Expr.Call (name, args) ->
    let checkArgs paramTys =
      let argLen = List.length args
      let arity = List.length paramTys

      if argLen <> arity then
        failwithf "Arity mismatch. Expected %d arguments but was %d (%s)" arity argLen name

      List.zip args paramTys
      |> List.map (fun (arg, paramTy) -> checkExpr state arg paramTy)

    match state.ValueEnv |> Map.tryFind name with
    | Some (ValueDef.Fn (fn, paramTys, _)) ->
      let args = checkArgs paramTys
      TExpr.Call(TCallable.Fn fn, args)

    | Some (ValueDef.Binary binary) when isComparison binary ->
      match args with
      | [ lhs; rhs ] ->
        let lhs = synthExpr state lhs
        let rhs = checkExpr state rhs (exprToTy state lhs)
        TExpr.Binary(binary, lhs, rhs)

      | _ -> unreachable ()

    | Some (ValueDef.Unary unary) ->
      let paramTy =
        match unary with
        | TUnary.Not -> TTy.Bool
        | TUnary.Minus -> TTy.Int
        | TUnary.ArrayLen -> unreachable ()

      match checkArgs [ paramTy ] with
      | [ arg ] -> TExpr.Unary(unary, arg)
      | _ -> unreachable ()

    | Some (ValueDef.Binary binary) ->
      let paramTys =
        match binary with
        | TBinary.Add
        | TBinary.Subtract
        | TBinary.Multiply
        | TBinary.Divide
        | TBinary.Modulo -> [ TTy.Int; TTy.Int ]

        | TBinary.Equal
        | TBinary.NotEqual
        | TBinary.LessThan
        | TBinary.LessEqual
        | TBinary.GreaterThan
        | TBinary.GreaterEqual -> unreachable ()

      match checkArgs paramTys with
      | [ l; r ] -> TExpr.Binary(binary, l, r)
      | _ -> unreachable ()

    | Some ((ValueDef.LogOr
    | ValueDef.LogAnd) as v) ->
      let c =
        match v with
        | ValueDef.LogOr -> TCallable.LogOr
        | ValueDef.LogAnd -> TCallable.LogAnd
        | _ -> unreachable ()

      let args = checkArgs [ TTy.Bool; TTy.Bool ]
      TExpr.Call(c, args)

    | Some ValueDef.ArrayPush ->
      match args with
      | [ array; item ] ->
        let array = synthExpr state array

        let itemTy =
          match exprToTy state array with
          | TTy.Array it -> it
          | ty -> mismatch (string ty) "array(_)"

        let item = checkExpr state item itemTy

        TExpr.Call(TCallable.ArrayPush, [ array; item ])

      | _ -> failwith "Expected 2 arguments (push)"

    | Some ValueDef.Assert ->
      match args with
      | [ cond ] ->
        let cond = checkExpr state cond TTy.Bool
        TExpr.Call(TCallable.Assert, [ cond ])

      | _ -> failwith "Expected 2 args (assert)"

    | Some (ValueDef.Constant _)
    | Some (ValueDef.Local _) -> failwithf "Can't call to %s" name

    | None -> failwithf "Undefined variable '%s'" name

  | S.Expr.Array []
  | S.Expr.Record _ -> failwithf "Can't infer type: %A." expr

let private checkExpr (state: TcState) (expr: S.Expr) (ty: TTy) : TExpr =
  match expr with
  | S.Expr.Array items ->
    let itemTy =
      match ty with
      | TTy.Array it -> it
      | _ -> mismatch (string ty) "array(_)"

    let items =
      items
      |> List.map (fun item -> checkExpr state item itemTy)

    TExpr.Array(itemTy, items)

  | S.Expr.Record fields ->
    let record =
      match ty with
      | TTy.Record it -> it
      | _ -> mismatch (string ty) "record"

    let mutable fieldMap =
      state.Records.[record.Index].Fields
      |> Array.map (fun ((f: Symbol), ty) -> f.Name, (f, ty))
      |> Map.ofArray

    let itemArray = ResizeArray()

    for field, rhs in fields do
      match fieldMap |> Map.tryFind field with
      | Some (f, fieldTy) ->
        itemArray.Add(f, checkExpr state rhs fieldTy)
        fieldMap <- Map.remove field fieldMap

      | _ -> failwithf "Field %s isn't a member of record '%A'" field record

    if fieldMap |> Map.isEmpty |> not then
      let rest =
        fieldMap
        |> Map.toList
        |> List.truncate 3
        |> List.map fst
        |> String.concat ", "

      failwithf "Missing fields (%s) from record '%A'" rest record

    let items =
      itemArray.ToArray()
      |> Array.sortBy fst
      |> Array.map snd
      |> Array.toList

    TExpr.Record(record, items)

  | _ ->
    let expr = synthExpr state expr
    unifyTy (exprToTy state expr) ty
    expr

let private checkStmt (state: TcState) (stmt: S.Stmt) : TStmt =
  match stmt with
  | S.Stmt.Do expr -> synthExpr state expr |> TStmt.Do

  | S.Stmt.Assign (place, value) ->
    let place = synthAsPlace state place
    let value = checkExpr state value (placeToTy state place)
    TStmt.Assign(place, value)

  | S.Stmt.Let (ident, tyOpt, init) ->
    let symbol =
      let index = state.Locals.Count
      newSymbol "_" index ident

    state.Locals.Add(symbol, TTy.Void)

    let init =
      match tyOpt with
      | Some ty -> checkExpr state init (checkTy state ty)
      | None -> synthExpr state init

    let ty = exprToTy state init

    state.Locals.[symbol.Index] <- (symbol, ty)

    state.ValueEnv <-
      state.ValueEnv
      |> Map.add ident (ValueDef.Local(symbol, ty))

    TStmt.Assign(TPlace.Local symbol, init)

  | S.Stmt.Break -> TStmt.Break
  | S.Stmt.Continue -> TStmt.Continue

  | S.Stmt.Return argOpt ->
    let arg =
      match state.ValueEnv |> Map.tryFind "__return" with
      | Some (ValueDef.Local (_, resultTy)) ->
        match argOpt with
        | Some arg -> checkExpr state arg resultTy

        | None ->
          try
            unifyTy resultTy TTy.Void
          with
          | _ -> failwithf "Use return statement with a value in non-void function"

          TExpr.Void

      | Some _ -> unreachable ()
      | None -> failwithf "Can't use return out of a function"

    TStmt.Return arg

  | S.Stmt.Block block ->
    let state = cloneState state

    let stmts =
      block.Stmts
      |> List.map (fun stmt -> checkStmt state stmt)

    let block: TBlock = { Stmts = stmts }
    TStmt.Block block

  | S.Stmt.If (cond, body, alt) ->
    let cond = checkExpr state cond TTy.Bool
    let body = checkStmt (cloneState state) body
    let alt = checkStmt (cloneState state) alt
    TStmt.If(cond, body, alt)

  | S.Stmt.Loop body -> TStmt.Loop(checkStmt state body)

let private checkDecl (state: TcState) (decl: S.Decl) : TDecl =
  match decl with
  | S.Decl.Block block ->
    let locals = ResizeArray([ newSymbol "_" 0 "__return", TTy.Void ])

    let innerState = { cloneState state with Locals = locals }

    let stmts =
      block.Stmts
      |> List.map (fun stmt -> withContext "statement" stmt (fun () -> checkStmt innerState stmt))

    let locals = innerState.Locals.ToArray() |> Array.toList

    let block: TBlock = { Stmts = stmts }
    TDecl.Block(locals, block)

  | S.Decl.Fn (name, paramList, resultTy, body) ->
    let index = state.Fns.Count
    let symbol = newSymbol "F" index name
    let locals = ResizeArray()

    let paramList =
      paramList
      |> List.mapi (fun i (ident, ty) -> newSymbol "_" (i + 1) ident, checkTy state ty)

    let paramTys = paramList |> List.map snd
    let resultTy = checkTy state resultTy

    let r = newSymbol "_" 0 "__return"
    locals.Add(r, TTy.Void)

    for p, ty in paramList do
      locals.Add(p, ty)

    let fnDef: FnDef =
      { ParamTys = paramTys
        ResultTy = resultTy }

    state.Fns.Add(fnDef)

    state.ValueEnv <-
      state.ValueEnv
      |> Map.add name (ValueDef.Fn(symbol, paramTys, resultTy))

    let innerState = { cloneState state with Locals = locals }

    for p, paramTy in paramList do
      innerState.ValueEnv <-
        innerState.ValueEnv
        |> Map.add p.Name (ValueDef.Local(p, paramTy))

    innerState.ValueEnv <-
      innerState.ValueEnv
      |> Map.add "__return" (ValueDef.Local(r, resultTy))

    let body = withContext "fn" name (fun () -> checkStmt innerState body)

    let locals = innerState.Locals.ToArray() |> Array.toList

    TDecl.Fn(symbol, paramList, resultTy, locals, body)

  | S.Decl.RecordTy (name, fields) ->
    let def: RecordTyDef = { Fields = Array.empty }

    let symbol =
      let index = state.Records.Count
      newSymbol "R" index name

    state.Records.Add(def)
    state.TyEnv <- state.TyEnv |> Map.add name (TTy.Record symbol)

    let fields =
      fields
      |> List.mapi (fun i (name, ty) -> newSymbol "F" i name, checkTy state ty)
      |> List.toArray

    state.Records.[symbol.Index] <- { Fields = fields }

    TDecl.RecordTy(symbol, fields)

let typeCheck (decls: S.Decl list) : TDecl list =
  let state = initialState ()

  decls
  |> List.map (fun decl -> checkDecl state decl)
