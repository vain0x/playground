module rec OptLang.TypeCheck

open OptLang.Symbol

module S = OptLang.Syntax
module T = OptLang.Tir

type Unary = T.Unary
type Binary = T.Binary

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ValueDef =
  | Constant of T.Expr
  | Local of Symbol * T.Ty
  | Fn of Symbol * T.Ty list * T.Ty
  | Unary of T.Unary
  | Binary of T.Binary
  | LogAnd
  | LogOr
  | ArrayPush
  | Assert

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type FnDef = { ParamTys: T.Ty list; ResultTy: T.Ty }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type RecordTyDef = { Fields: (Symbol * T.Ty) array }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TcState =
  { mutable ValueEnv: Map<string, ValueDef>
    mutable TyEnv: Map<string, T.Ty>
    Locals: ResizeArray<Symbol * T.Ty>
    Fns: ResizeArray<FnDef>
    Records: ResizeArray<RecordTyDef> }

let private builtInValueEnv =
  [ "void", ValueDef.Constant T.Expr.Void
    "false", ValueDef.Constant(T.Expr.Bool false)
    "true", ValueDef.Constant(T.Expr.Bool true)
    "not", ValueDef.Unary T.Unary.Not
    "minus", ValueDef.Unary T.Unary.Minus

    "||", ValueDef.LogOr
    "&&", ValueDef.LogAnd
    "==", ValueDef.Binary T.Binary.Equal
    "!=", ValueDef.Binary T.Binary.NotEqual
    "<", ValueDef.Binary T.Binary.LessThan
    "<=", ValueDef.Binary T.Binary.LessEqual
    ">", ValueDef.Binary T.Binary.GreaterThan
    ">=", ValueDef.Binary T.Binary.GreaterEqual
    "+", ValueDef.Binary T.Binary.Add
    "-", ValueDef.Binary T.Binary.Subtract
    "*", ValueDef.Binary T.Binary.Multiply
    "/", ValueDef.Binary T.Binary.Divide
    "%", ValueDef.Binary T.Binary.Modulo

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

let private isComparison (binary: T.Binary) =
  match binary with
  | Binary.Equal
  | Binary.NotEqual
  | Binary.LessThan
  | Binary.LessEqual
  | Binary.GreaterThan
  | Binary.GreaterEqual -> true
  | _ -> false

let private unreachable () = failwith "unreachable"

let private mismatch actual expected =
  failwithf "Type mismatch. Expected %s but was %s" expected actual

let private unifyTy (actualTy: T.Ty) (expectedTy: T.Ty) =
  if actualTy <> expectedTy then
    failwithf "Type mismatch. Expected %A but was %A" expectedTy actualTy

let private placeToTy (state: TcState) (place: T.Place) : T.Ty =
  match place with
  | T.Place.Local symbol ->
    let _, ty = state.Locals.[symbol.Index]
    ty

  | T.Place.Index (lhs, _) ->
    match placeToTy state lhs with
    | T.Ty.Array itemTy -> itemTy
    | _ -> unreachable ()

  | T.Place.Field (lhs, field) ->
    match placeToTy state lhs with
    | T.Ty.Record record ->
      state.Records.[record.Index].Fields.[field.Index]
      |> snd

    | _ -> unreachable ()

let private exprToTy (state: TcState) (expr: T.Expr) : T.Ty =
  match expr with
  | T.Expr.Void -> T.Ty.Void
  | T.Expr.Bool _ -> T.Ty.Bool
  | T.Expr.Int _ -> T.Ty.Int
  | T.Expr.String _ -> T.Ty.String
  | T.Expr.Read place -> placeToTy state place

  | T.Expr.Array (itemTy, _) -> T.Ty.Array itemTy
  | T.Expr.Record (record, _) -> T.Ty.Record record

  | T.Expr.Unary (unary, _) ->
    match unary with
    | Unary.Not -> T.Ty.Bool

    | Unary.Minus
    | Unary.ArrayLen -> T.Ty.Int

  | T.Expr.Binary (binary, _, _) ->
    match binary with
    | Binary.Add
    | Binary.Subtract
    | Binary.Multiply
    | Binary.Divide
    | Binary.Modulo -> T.Ty.Int

    | Binary.Equal
    | Binary.NotEqual
    | Binary.LessThan
    | Binary.LessEqual
    | Binary.GreaterThan
    | Binary.GreaterEqual -> T.Ty.Bool

  | T.Expr.Call (callable, _) ->
    match callable with
    | T.Callable.Fn fn -> state.Fns.[fn.Index].ResultTy

    | T.Callable.LogOr
    | T.Callable.LogAnd -> T.Ty.Bool

    | T.Callable.ArrayPush
    | T.Callable.Assert -> T.Ty.Void

let private checkTy (state: TcState) (ty: S.Ty) : T.Ty =
  match ty with
  | S.Ty.Void -> T.Ty.Void
  | S.Ty.Bool -> T.Ty.Bool
  | S.Ty.Int -> T.Ty.Int
  | S.Ty.String -> T.Ty.String

  | S.Ty.Name name ->
    match state.TyEnv |> Map.tryFind name with
    | Some ty -> ty
    | None -> failwithf "Undefined type '%s'" name

  | S.Ty.Array itemTy -> T.Ty.Array(checkTy state itemTy)

let private synthFieldAsPlace (state: TcState) expr : Result<T.Place, T.Expr> =
  let lhs, field =
    match expr with
    | S.Expr.Field (lhs, field) -> lhs, field
    | _ -> unreachable ()

  let lhs = synthAsPlace state lhs

  match placeToTy state lhs with
  | T.Ty.Record record ->
    let fields = state.Records.[record.Index].Fields

    match fields
          |> Array.tryPick (fun ((f: Symbol), _) -> if f.Name = field then Some f else None)
      with
    | Some field -> Ok(T.Place.Field(lhs, field))
    | None -> failwithf "Missing field '%s' in a record type '%A'" field record

  | T.Ty.Array _ ->
    match field with
    | "len" -> Error(T.Expr.Unary(Unary.ArrayLen, T.Expr.Read lhs))
    | _ -> failwithf "Array doesn't have field '%s'" field

  | _ -> mismatch (string (placeToTy state lhs)) "record"

let private synthAsPlace (state: TcState) (expr: S.Expr) : T.Place =
  match expr with
  | S.Expr.Name name ->
    match state.ValueEnv |> Map.tryFind name with
    | Some (ValueDef.Local (local, _)) -> T.Place.Local local
    | Some _ -> failwithf "Function can't be used as a value '%s'" name
    | None -> failwithf "Undefined variable '%s'" name

  | S.Expr.Index (lhs, index) ->
    let lhs = synthAsPlace state lhs

    match placeToTy state lhs with
    | T.Ty.Array _ -> ()
    | ty -> mismatch (string ty) "array(_)"

    let index = checkExpr state index T.Ty.Int
    T.Place.Index(lhs, index)

  | S.Expr.Field _ ->
    match synthFieldAsPlace state expr with
    | Ok it -> it
    | Error _ -> failwithf "Expected a place but was %A" expr

  | _ -> failwithf "Expected a place but was %A" expr

let private checkAsPlace (state: TcState) (expr: S.Expr) (ty: T.Ty) : T.Place =
  match expr with
  | S.Expr.Index (lhs, index) ->
    let lhs = checkAsPlace state lhs (T.Ty.Array ty)
    let index = checkExpr state index T.Ty.Int
    T.Place.Index(lhs, index)

  | S.Expr.Name _
  | S.Expr.Field _ ->
    let place = synthAsPlace state expr
    unifyTy (placeToTy state place) ty
    place

  | _ -> failwithf "Expected a place but was %A" expr

let private synthExpr (state: TcState) (expr: S.Expr) : T.Expr =
  match expr with
  | S.Expr.Name name ->
    match state.ValueEnv |> Map.tryFind name with
    | Some (ValueDef.Constant expr) -> expr
    | _ ->
      let place = synthAsPlace state expr
      T.Expr.Read place

  | S.Expr.Int value -> T.Expr.Int value
  | S.Expr.String value -> T.Expr.String value

  | S.Expr.Array (head :: tail) ->
    let head = synthExpr state head
    let itemTy = exprToTy state head

    let tail =
      tail
      |> List.map (fun item -> checkExpr state item itemTy)

    T.Expr.Array(itemTy, head :: tail)

  | S.Expr.Index _ -> T.Expr.Read(synthAsPlace state expr)

  | S.Expr.Field _ ->
    match synthFieldAsPlace state expr with
    | Ok place -> T.Expr.Read place
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
      T.Expr.Call(T.Callable.Fn fn, args)

    | Some (ValueDef.Binary binary) when isComparison binary ->
      match args with
      | [ lhs; rhs ] ->
        let lhs = synthExpr state lhs
        let rhs = checkExpr state rhs (exprToTy state lhs)
        T.Expr.Binary(binary, lhs, rhs)

      | _ -> unreachable ()

    | Some (ValueDef.Unary unary) ->
      let paramTy =
        match unary with
        | Unary.Not -> T.Ty.Bool
        | Unary.Minus -> T.Ty.Int
        | Unary.ArrayLen -> unreachable ()

      match checkArgs [ paramTy ] with
      | [ arg ] -> T.Expr.Unary(unary, arg)
      | _ -> unreachable ()

    | Some (ValueDef.Binary binary) ->
      let paramTys =
        match binary with
        | Binary.Add
        | Binary.Subtract
        | Binary.Multiply
        | Binary.Divide
        | Binary.Modulo -> [ T.Ty.Int; T.Ty.Int ]

        | Binary.Equal
        | Binary.NotEqual
        | Binary.LessThan
        | Binary.LessEqual
        | Binary.GreaterThan
        | Binary.GreaterEqual -> unreachable ()

      match checkArgs paramTys with
      | [ l; r ] -> T.Expr.Binary(binary, l, r)
      | _ -> unreachable ()

    | Some ((ValueDef.LogOr
    | ValueDef.LogAnd) as v) ->
      let c =
        match v with
        | ValueDef.LogOr -> T.Callable.LogOr
        | ValueDef.LogAnd -> T.Callable.LogAnd
        | _ -> unreachable ()

      let args = checkArgs [ T.Ty.Bool; T.Ty.Bool ]
      T.Expr.Call(c, args)

    | Some ValueDef.ArrayPush ->
      match args with
      | [ array; item ] ->
        let array = synthExpr state array

        let itemTy =
          match exprToTy state array with
          | T.Ty.Array it -> it
          | ty -> mismatch (string ty) "array(_)"

        let item = checkExpr state item itemTy

        T.Expr.Call(T.Callable.ArrayPush, [ array; item ])

      | _ -> failwith "Expected 2 arguments (push)"

    | Some ValueDef.Assert ->
      match args with
      | [ cond ] ->
        let cond = checkExpr state cond T.Ty.Bool
        T.Expr.Call(T.Callable.Assert, [ cond ])

      | _ -> failwith "Expected 2 args (assert)"

    | Some (ValueDef.Constant _)
    | Some (ValueDef.Local _) -> failwithf "Can't call to %s" name

    | None -> failwithf "Undefined variable '%s'" name

  | S.Expr.Array []
  | S.Expr.Record _ -> failwithf "Can't infer type: %A." expr

let private checkExpr (state: TcState) (expr: S.Expr) (ty: T.Ty) : T.Expr =
  match expr with
  | S.Expr.Array items ->
    let itemTy =
      match ty with
      | T.Ty.Array it -> it
      | _ -> mismatch (string ty) "array(_)"

    let items =
      items
      |> List.map (fun item -> checkExpr state item itemTy)

    T.Expr.Array(itemTy, items)

  | S.Expr.Record fields ->
    let record =
      match ty with
      | T.Ty.Record it -> it
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

    T.Expr.Record(record, items)

  | _ ->
    let expr = synthExpr state expr
    unifyTy (exprToTy state expr) ty
    expr

let private checkStmt (state: TcState) (stmt: S.Stmt) : T.Stmt =
  match stmt with
  | S.Stmt.Do expr -> synthExpr state expr |> T.Stmt.Do

  | S.Stmt.Assign (place, value) ->
    let place = synthAsPlace state place
    let value = checkExpr state value (placeToTy state place)
    T.Stmt.Assign(place, value)

  | S.Stmt.Let (ident, tyOpt, init) ->
    let symbol =
      let index = state.Locals.Count
      newSymbol "_" index ident

    state.Locals.Add(symbol, T.Ty.Void)

    let init =
      match tyOpt with
      | Some ty -> checkExpr state init (checkTy state ty)
      | None -> synthExpr state init

    let ty = exprToTy state init

    state.Locals.[symbol.Index] <- (symbol, ty)

    state.ValueEnv <-
      state.ValueEnv
      |> Map.add ident (ValueDef.Local(symbol, ty))

    T.Stmt.Assign(T.Place.Local symbol, init)

  | S.Stmt.Break -> T.Stmt.Break
  | S.Stmt.Continue -> T.Stmt.Continue

  | S.Stmt.Return argOpt ->
    let arg =
      match state.ValueEnv |> Map.tryFind "__return" with
      | Some (ValueDef.Local (_, resultTy)) ->
        match argOpt with
        | Some arg -> checkExpr state arg resultTy

        | None ->
          try
            unifyTy resultTy T.Ty.Void
          with
          | _ -> failwithf "Use return statement with a value in non-void function"

          T.Expr.Void

      | Some _ -> unreachable ()
      | None -> failwithf "Can't use return out of a function"

    T.Stmt.Return arg

  | S.Stmt.Block block ->
    let state = cloneState state

    let stmts =
      block.Stmts
      |> List.map (fun stmt -> checkStmt state stmt)

    let block: T.Block = { Stmts = stmts }
    T.Stmt.Block block

  | S.Stmt.If (cond, body, alt) ->
    let cond = checkExpr state cond T.Ty.Bool
    let body = checkStmt (cloneState state) body
    let alt = checkStmt (cloneState state) alt
    T.Stmt.If(cond, body, alt)

  | S.Stmt.Loop body -> T.Stmt.Loop(checkStmt state body)

let private checkDecl (state: TcState) (decl: S.Decl) : T.Decl =
  match decl with
  | S.Decl.Block block ->
    let locals = ResizeArray([ newSymbol "_" 0 "__return", T.Ty.Void ])

    let innerState = { cloneState state with Locals = locals }

    let stmts =
      block.Stmts
      |> List.map (fun stmt ->
        try
          checkStmt innerState stmt
        with
        | _ ->
          eprintfn "In statement: %A" stmt
          reraise ())

    let locals = innerState.Locals.ToArray() |> Array.toList

    let block: T.Block = { Stmts = stmts }
    T.Decl.Block(locals, block)

  | S.Decl.Fn (name, paramList, resultTy, body) ->
    let index = state.Fns.Count
    let symbol = newSymbol "F" index name
    let locals = ResizeArray()

    let paramList =
      paramList
      |> List.mapi (fun i (ident, ty) -> newSymbol "P" (i + 1) ident, checkTy state ty)

    let paramTys = paramList |> List.map snd
    let resultTy = checkTy state resultTy

    let r = newSymbol "_" 0 "__return"
    locals.Add(r, T.Ty.Void)

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

    let body =
      try
        checkStmt innerState body
      with
      | _ ->
        eprintfn "In fn: %s" name
        reraise ()

    let locals = innerState.Locals.ToArray() |> Array.toList

    T.Decl.Fn(symbol, paramList, resultTy, locals, body)

  | S.Decl.RecordTy (name, fields) ->
    let def: RecordTyDef = { Fields = Array.empty }

    let symbol =
      let index = state.Records.Count
      newSymbol "R" index name

    state.Records.Add(def)
    state.TyEnv <- state.TyEnv |> Map.add name (T.Ty.Record symbol)

    let fields =
      fields
      |> List.mapi (fun i (name, ty) -> newSymbol "F" i name, checkTy state ty)
      |> List.toArray

    state.Records.[symbol.Index] <- { Fields = fields }

    T.Decl.RecordTy(symbol, fields)

let typeCheck (decls: S.Decl list) : T.Decl list =
  let state = initialState ()

  decls
  |> List.map (fun decl -> checkDecl state decl)
