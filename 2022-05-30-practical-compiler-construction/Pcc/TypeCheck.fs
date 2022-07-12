module rec Pcc.TypeCheck

open Pcc.Ast

let inline private unreachable () = failwith "unreachable"

let lookup (name: string) env =
  let rec go env =
    match env with
    | (key, value) :: _ when key = name -> Some value

    | []
    | ("@", _) :: _ -> None

    | _ :: env -> go env

  go env

// -----------------------------------------------
// 中間表現
// -----------------------------------------------

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type VarInfo = { Ty: Ty; Offset: int; Level: int }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type FunInfo =
  { Formals: Ty list
    Result: Ty
    Level: int }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Entry =
  | Var of VarInfo
  | Fun of FunInfo

[<ReferenceEquality>]
type Tag = Tag of unit ref

[<RequireQualifiedAccess>]
type Ty =
  | Int
  | Str
  | Array of len: int * Tag
  | Name of string * Ty option ref
  | Unit

let createTy tEnv ty =
  match ty with
  | Typ.Name name ->
    match lookup name tEnv with
    | Some it -> it, tEnv
    | None ->
      let ty = Ty.Name(name, ref None)
      ty, (name, ty) :: tEnv

  | Typ.Array len -> Ty.Array(len, Tag(ref ())), tEnv
  | Typ.Int -> Ty.Int, tEnv
  | Typ.Void -> Ty.Unit, tEnv

let private actualTy ty =
  let rec go visited ty =
    match ty with
    | Ty.Name (name, optRef) ->
      match optRef.contents with
      | Some (Ty.Name (name, _)) when Set.contains name visited -> failwithf "Cyclic type '%s'" name

      | Some ty ->
        let ty = go (Set.add name visited) ty
        optRef.contents <- Some ty
        ty

      | None -> failwithf "No actual type of '%s'" name

    | _ -> ty

  go Set.empty ty

let private checkInt ty =
  match ty with
  | Ty.Int -> ()
  | _ -> failwithf "Expected an int"

let private checkType ty expectedTy =
  match actualTy ty, actualTy expectedTy with
  | Ty.Name _, _
  | _, Ty.Name _ -> unreachable ()

  | Ty.Int, Ty.Int -> ()
  | Ty.Str, Ty.Str -> ()
  | Ty.Unit, Ty.Unit -> ()

  | Ty.Array (lLen, _), Ty.Array (rLen, _) ->
    if lLen <> rLen then
      failwithf "Array length mismatch (%d, %d)" lLen rLen

  | lTy, rTy -> failwithf "Type mismatch (%A <-> %A)" lTy rTy

// 未実装
let private checkRedecl (_decs: Dec list) (_env: (string * Entry) list) = ()

let private typeExpr ast env =
  match ast with
  | Expr.Num _ -> Ty.Int
  | Expr.Var v -> typeVar v env
  | Expr.Str _ -> failwith "String expression must appear as an argument of sprint."

  | Expr.Call ("new", aargs) ->
    match aargs with
    | [ Expr.Var (Var name) ] ->
      match typeVar (Var name) env with
      | Ty.Array _ -> Ty.Unit
      | _ -> failwithf "Expected an array '%s'" name

    | _ -> failwithf "Invalid arguments of 'new' call"

  | Expr.Call ("scan", aargs) ->
    match aargs with
    | [ Expr.Var (Var name) ] ->
      checkInt (typeVar (Var name) env)
      Ty.Unit

    | _ -> failwith "Invalid arguments of 'scan' call"

  | Expr.Call (name, aargs) ->
    match lookup name env with
    | Some (Entry.Fun fi) ->
      if List.length fi.Formals <> List.length aargs then
        failwithf "Incorrect arity (%s)" name

      let argTys = aargs |> List.map (fun arg -> typeExpr arg env)

      for aarg, farg in List.zip argTys fi.Formals do
        checkType aarg farg

      fi.Result

    | Some (Entry.Var _) -> failwithf "Can't call non-function '%s'" name
    | None -> failwithf "No such function '%s'" name

let typeVar v env =
  match v with
  | Var name ->
    match lookup name env with
    | Some (Entry.Var vi) -> actualTy vi.Ty
    | _ -> failwithf "Undefined variable '%s'" name

  | IndexedVar (name, index) ->
    let arrayTy = typeVar (Var name) env

    match arrayTy with
    | Ty.Array _ ->
      checkInt (typeExpr index env)
      Ty.Int

    | _ -> failwithf "Expected an array '%s'" name

let typeStmt ast env =
  match ast with
  | Stmt.Assign (v, rhs) ->
    let varTy = typeVar v env
    let rhsTy = typeExpr rhs env
    checkType rhsTy varTy

  | Stmt.CallProc (name, aargs) -> typeExpr (Expr.Call(name, aargs)) env |> ignore

  | Stmt.Block (decs, _) -> checkRedecl decs env
  | Stmt.If (cond, _, _) -> typeCond cond env
  | Stmt.While (cond, _) -> typeCond cond env
  | Stmt.Nil -> ()

let typeParamDec ast nest tEnv env =
  ast
  |> List.fold
       (fun (i, env) (typ, name) ->
         let ty, _ = createTy tEnv typ

         let vi: VarInfo =
           { Ty = ty
             Level = nest
             Offset = i * 8 + 16 }

         i + 1, (name, Entry.Var vi) :: env)
       (0, env)
  |> snd

let typeDec ast nest tEnv env addr =
  match ast with
  | Dec.Func (name, fargs, resultTy, _) ->
    let formals, tEnv =
      fargs
      |> List.mapFold (fun tEnv (ty, _) -> createTy tEnv ty) tEnv

    let result, tEnv = createTy tEnv resultTy

    let fi: FunInfo =
      { Formals = formals
        Result = result
        Level = nest + 1 }

    tEnv, (name, Entry.Fun fi) :: env, addr

  | Dec.Type (name, ty) ->
    let ty, tEnv = createTy tEnv ty
    (name, ty) :: tEnv, env, addr

  | Dec.Var (ty, name) ->
    let ty, tEnv = createTy tEnv ty
    let addr = addr - 8
    let vi: VarInfo = { Ty = ty; Offset = addr; Level = nest }

    tEnv, ((name, Entry.Var vi) :: env), addr

let typeDecs decs nest tEnv env =
  decs
  |> List.fold (fun (tEnv, env, addr) dec -> typeDec dec nest tEnv env addr) (tEnv, env, 0)

let private typeCond ast env =
  match ast with
  | Expr.Call (_, [ l; r ]) ->
    checkInt (typeExpr l env)
    checkInt (typeExpr r env)

  | _ -> unreachable ()

let builtInTEnv () =
  [ "int", Ty.Int
    "str", Ty.Str
    "void", Ty.Unit ]

let builtInEnv () =
  let newFi formals result : Entry =
    let fi: FunInfo =
      { Formals = formals
        Result = result
        Level = 0 }

    Entry.Fun fi

  let entries =
    [ "+", newFi [ Ty.Int; Ty.Int ] Ty.Int
      "-", newFi [ Ty.Int; Ty.Int ] Ty.Int
      "*", newFi [ Ty.Int; Ty.Int ] Ty.Int
      "/", newFi [ Ty.Int; Ty.Int ] Ty.Int
      "!", newFi [ Ty.Int ] Ty.Int
      "==", newFi [ Ty.Int; Ty.Int ] Ty.Int
      "!=", newFi [ Ty.Int; Ty.Int ] Ty.Int
      "<", newFi [ Ty.Int; Ty.Int ] Ty.Int
      "<=", newFi [ Ty.Int; Ty.Int ] Ty.Int
      ">", newFi [ Ty.Int; Ty.Int ] Ty.Int
      ">=", newFi [ Ty.Int; Ty.Int ] Ty.Int
      "iprint", newFi [ Ty.Int ] Ty.Unit
      "sprint", newFi [ Ty.Str ] Ty.Unit ]

  entries
