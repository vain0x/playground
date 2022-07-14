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

/// 型の環境
type TEnv = (string * Ty) list

/// 値の環境
type Env = (string * Entry) list

/// 構文上の型を中間表現の型に変換する
let createTy (tEnv: TEnv) (typ: Typ) : Ty =
  match typ with
  | Typ.Name name ->
    match lookup name tEnv with
    | Some it -> it
    | None -> failwithf "Undefined type '%s'" name

  | Typ.Array len -> Ty.Array(len, Tag(ref ()))
  | Typ.Int -> Ty.Int
  | Typ.Void -> Ty.Unit

/// 名前付きの型を展開する
let private actualTy ty =
  let rec go visited ty =
    match ty with
    | Ty.Name (name, tyOptRef) ->
      match tyOptRef.contents with
      | Some (Ty.Name (name, _)) when Set.contains name visited -> failwithf "Cyclic type '%s'" name

      | Some ty ->
        // 経路圧縮
        let ty = go (Set.add name visited) ty
        tyOptRef.contents <- Some ty
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
let private checkRedecl (_: Dec list) (_: Env) = ()

/// 式の型検査
let private typeExpr ast env =
  match ast with
  | Expr.Num _ -> Ty.Int
  | Expr.Str _ -> Ty.Str
  | Expr.Var v -> typeVar v env

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

  | Expr.Call ("return", [ arg ]) ->
    // TODO
    let _ = typeExpr arg env

    Ty.Unit

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

/// 変数を型検査する
let typeVar (v: Var) (env: Env) : Ty =
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

/// 文を型検査する
let typeStmt (ast: Stmt) (env: Env) : unit =
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

/// パラメータを環境に加える
let typeParamDec (ast: (Typ * string) list) (nest: int) (tEnv: TEnv) (env: Env) : Env =
  // フレームポインタに対するパラメータのオフセットは24から始まる
  // (スタックポインタ、静的リンク、戻りアドレスの後ろにパラメータが積まれるため)
  let savedArg = 24

  ast
  |> List.fold
       (fun (addr, env) (typ, name) ->
         let ty = createTy tEnv typ

         let vi: VarInfo = { Ty = ty; Level = nest; Offset = addr }

         addr + 8, (name, Entry.Var vi) :: env)
       (savedArg, env)
  |> snd

/// 宣言を型検査する
///
/// - この関数はブロックをコード生成する際に呼ばれる
///     - 内部の文や宣言に対する型検査は、そのブロックに到達したときに行う
///     - この関数は再帰的に内部の文や宣言に入らない
let typeDec (ast: Dec) (nest: int) (tEnv: TEnv) (env: Env) (addr: int) : TEnv * Env * int =
  match ast with
  | Dec.Func (name, fargs, resultTy, _) ->
    let formals =
      fargs
      |> List.map (fun (ty, _) -> createTy tEnv ty)

    let fi: FunInfo =
      { Formals = formals
        Result = createTy tEnv resultTy
        Level = nest + 1 }

    tEnv, (name, Entry.Fun fi) :: env, addr

  | Dec.Type (name, _) ->
    // 具体的な型は transDec で解決する
    let ty = Ty.Name(name, ref None)
    (name, ty) :: tEnv, env, addr

  | Dec.Var (ty, name) ->
    let ty = createTy tEnv ty
    let addr = addr - 8
    let vi: VarInfo = { Ty = ty; Offset = addr; Level = nest }

    tEnv, ((name, Entry.Var vi) :: env), addr

let typeDecs decs nest tEnv env : TEnv * Env * int =
  decs
  |> List.fold (fun (tEnv, env, addr) dec -> typeDec dec nest tEnv env addr) (tEnv, env, 0)

let private typeCond ast env =
  match ast with
  | Expr.Call (_, [ l; r ]) ->
    checkInt (typeExpr l env)
    checkInt (typeExpr r env)

  | _ -> unreachable ()

let builtInTEnv () : TEnv =
  [ "int", Ty.Int
    "str", Ty.Str
    "void", Ty.Unit ]

let builtInEnv () : Env =
  let newFi formals result : Entry =
    let fi: FunInfo =
      { Formals = formals
        Result = result
        Level = 0 }

    Entry.Fun fi

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
