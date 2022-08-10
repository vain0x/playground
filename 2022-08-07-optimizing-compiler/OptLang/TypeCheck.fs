module rec OptLang.TypeCheck

open OptLang.Parse
open OptLang.Syntax

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type BuiltIn =
  | Compare
  | Push

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ValueDef =
  | Local of Ty
  | Fn of Ty list * Ty
  | BuiltIn of BuiltIn

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type RecordTyDef = { Field: (string * Ty) list }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TcState =
  { mutable ValueEnv: Map<string, ValueDef>
    mutable TyEnv: Map<string, RecordTyDef> }

let private initialState: TcState =
  { ValueEnv =
      [ "not", ValueDef.Fn([ Ty.Bool ], Ty.Bool)
        "minus", ValueDef.Fn([ Ty.Int ], Ty.Int)

        "||", ValueDef.Fn([ Ty.Bool; Ty.Bool ], Ty.Bool)
        "&&", ValueDef.Fn([ Ty.Bool; Ty.Bool ], Ty.Bool)
        "==", ValueDef.BuiltIn BuiltIn.Compare
        "!=", ValueDef.BuiltIn BuiltIn.Compare
        "<", ValueDef.BuiltIn BuiltIn.Compare
        "<=", ValueDef.BuiltIn BuiltIn.Compare
        ">", ValueDef.BuiltIn BuiltIn.Compare
        ">=", ValueDef.BuiltIn BuiltIn.Compare
        "+", ValueDef.Fn([ Ty.Int; Ty.Int ], Ty.Int)
        "-", ValueDef.Fn([ Ty.Int; Ty.Int ], Ty.Int)
        "*", ValueDef.Fn([ Ty.Int; Ty.Int ], Ty.Int)
        "/", ValueDef.Fn([ Ty.Int; Ty.Int ], Ty.Int)
        "%", ValueDef.Fn([ Ty.Int; Ty.Int ], Ty.Int)

        "push", ValueDef.BuiltIn BuiltIn.Push
        "assert", ValueDef.Fn([ Ty.Bool ], Ty.Void) ]
      |> Map.ofList
    TyEnv = Map.empty }

let private cloneState (state: TcState) : TcState =
  { ValueEnv = state.ValueEnv
    TyEnv = state.TyEnv }

let private unreachable () = failwith "unreachable"

let private mismatch actual expected =
  failwithf "Type mismatch. Expected %s but was %s" expected actual

let private checkTy (actualTy: Ty) (expectedTy: Ty) =
  if actualTy <> expectedTy then
    failwithf "Type mismatch. Expected %A but was %A" expectedTy actualTy

let private synthExpr (state: TcState) (expr: Expr) =
  match expr with
  | Expr.Name name ->
    match state.ValueEnv |> Map.tryFind name with
    | Some (ValueDef.Local ty) -> ty
    | Some _ -> failwithf "Function can't be used as a value '%s'" name
    | None -> failwithf "Undefined variable '%s'" name

  | Expr.Int _ -> Ty.Int
  | Expr.String _ -> Ty.String

  | Expr.Array (head :: tail) ->
    let itemTy = synthExpr state head

    for item in tail do
      checkExpr state item itemTy

    Ty.Array itemTy

  | Expr.Index (lhs, index) ->
    let arrayTy = synthExpr state lhs

    let itemTy =
      match arrayTy with
      | Ty.Array it -> it
      | _ -> mismatch (string arrayTy) "array(_)"

    checkExpr state index Ty.Int
    itemTy

  | Expr.Field (lhs, field) ->
    let lhsTy = synthExpr state lhs

    match lhsTy with
    | Ty.Name name ->
      match state.TyEnv |> Map.tryFind name with
      | Some def ->
        match def.Field
              |> List.tryPick (fun (f, ty) -> if f = field then Some ty else None)
          with
        | Some ty -> ty
        | None -> failwithf "Missing field '%s' in a record type '%s'" field name

      | _ -> failwithf "Undefined type '%s'" name

    | Ty.Array _ ->
      match field with
      | "len" -> Ty.Int
      | _ -> failwithf "Array doesn't have field '%s'" field

    | _ -> mismatch (string lhsTy) "record"

  | Expr.Call (name, args) ->
    match state.ValueEnv |> Map.tryFind name with
    | Some (ValueDef.Fn (paramTys, resultTy)) ->
      let argLen = List.length args
      let arity = List.length paramTys

      if argLen <> arity then
        failwithf "Arity mismatch. Expected %d arguments but was %d (%s)" arity argLen name

      for arg, paramTy in List.zip args paramTys do
        checkExpr state arg paramTy

      resultTy

    | Some (ValueDef.BuiltIn BuiltIn.Compare) ->
      match args with
      | [ left; right ] ->
        let ty = synthExpr state left
        checkExpr state right ty
        Ty.Bool

      | _ -> unreachable ()

    | Some (ValueDef.BuiltIn BuiltIn.Push) ->
      match args with
      | [ array; item ] ->
        let arrayTy = synthExpr state array

        let itemTy =
          match arrayTy with
          | Ty.Array it -> it
          | _ -> mismatch (string arrayTy) "array(_)"

        checkExpr state item itemTy
        Ty.Void

      | _ -> failwith "Expected 2 arguments (push)"

    | Some (ValueDef.Local _) -> failwithf "Variable can't be called ('%s')" name

    | None -> failwithf "Undefined variable '%s'" name

  | Expr.Array []
  | Expr.Record _ -> failwithf "Can't infer type: %A." expr

let private checkExpr (state: TcState) (expr: Expr) (ty: Ty) =
  match expr with
  | Expr.Array items ->
    let itemTy =
      match ty with
      | Ty.Array it -> it
      | _ -> mismatch (string ty) "array(_)"

    for item in items do
      checkExpr state item itemTy

  | Expr.Record fields ->
    let name =
      match ty with
      | Ty.Name it -> it
      | _ -> mismatch (string ty) "record"

    let recordTyDef =
      state.TyEnv
      |> Map.tryFind name
      |> Option.defaultWith unreachable

    let mutable fieldMap = Map.ofList recordTyDef.Field

    for field, rhs in fields do
      match fieldMap |> Map.tryFind field with
      | Some fieldTy ->
        checkExpr state rhs fieldTy
        fieldMap <- Map.remove field fieldMap

      | _ -> failwithf "Field %s isn't a member of record '%s'" field name

    match fieldMap
          |> Map.toList
          |> List.truncate 3
          |> List.map fst
      with
    | [] -> ()
    | rest -> failwithf "Missing fields (%s) from record '%s'" (rest |> String.concat ", ") name

  | Expr.Index (lhs, index) ->
    checkExpr state lhs (Ty.Array ty)
    checkExpr state index Ty.Int

  | Expr.Int _
  | Expr.String _
  | Expr.Name _
  | Expr.Field _
  | Expr.Call _ -> checkTy ty (synthExpr state expr)

// let private checkPlace (state: TcState) place =
//   match place with

let private checkStmt (state: TcState) (stmt: Stmt) =
  match stmt with
  | Stmt.Do expr -> synthExpr state expr |> ignore

  | Stmt.Assign (place, value) ->
    let ty = synthExpr state place
    checkExpr state value ty

  | Stmt.Let (ident, tyOpt, init) ->
    let ty =
      match tyOpt with
      | Some ty ->
        checkExpr state init ty
        ty

      | None -> synthExpr state init

    state.ValueEnv <-
      state.ValueEnv
      |> Map.add ident (ValueDef.Local ty)

  | Stmt.Break -> ()
  | Stmt.Continue -> ()

  | Stmt.Return argOpt ->
    match state.ValueEnv |> Map.tryFind "__return" with
    | Some (ValueDef.Local resultTy) ->
      match argOpt with
      | Some arg -> checkExpr state arg resultTy

      | None ->
        try
          checkTy resultTy Ty.Void
        with
        | _ -> failwithf "Use return statement with a value in non-void function"

    | Some _ -> unreachable ()
    | None -> failwithf "Can't use return out of a function"

  | Stmt.Block block ->
    let state = cloneState state

    for stmt in block.Stmts do
      checkStmt state stmt

  | Stmt.If (cond, body, alt) ->
    checkExpr state cond Ty.Bool
    checkStmt (cloneState state) body
    checkStmt (cloneState state) alt

  | Stmt.Loop body -> checkStmt state body

let private checkDecl (state: TcState) (decl: Decl) =
  match decl with
  | Decl.Block block ->
    let innerState = cloneState state

    for stmt in block.Stmts do
      try
        checkStmt innerState stmt
      with
      | _ ->
        eprintfn "In statement: %A" stmt
        reraise ()

  | Decl.Fn (name, paramList, resultTy, body) ->
    // FIXME: validate type ascriptions

    let paramTys = paramList |> List.map snd

    state.ValueEnv <-
      state.ValueEnv
      |> Map.add name (ValueDef.Fn(paramTys, resultTy))

    let innerState = cloneState state

    for paramName, paramTy in paramList do
      innerState.ValueEnv <-
        innerState.ValueEnv
        |> Map.add paramName (ValueDef.Local paramTy)

    innerState.ValueEnv <-
      innerState.ValueEnv
      |> Map.add "__return" (ValueDef.Local resultTy)

    try
      checkStmt innerState body
    with
    | _ ->
      eprintfn "In fn: %s" name
      reraise ()

  | Decl.RecordTy (name, fields) ->
    let def: RecordTyDef = { Field = fields }
    state.TyEnv <- state.TyEnv |> Map.add name def

let typeCheck (decls: Decl list) =
  let state = initialState

  for decl in decls do
    checkDecl state decl

  state
