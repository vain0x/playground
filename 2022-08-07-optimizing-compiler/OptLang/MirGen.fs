module rec OptLang.MirGen

open OptLang.Mir
open OptLang.Symbol
open OptLang.Tir
open System.Collections.Generic

module TC = OptLang.TypeCheck

let inline private unreachable () = failwith "unreachable"

let inline private unwrap opt =
  match opt with
  | Some it -> it
  | None -> unreachable ()

// -----------------------------------------------
// MIR Helpers
// -----------------------------------------------

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private ValueDef =
  | Local of LocalDef
  | Fn of FnDef
  | Unary of MUnary
  | Binary of MBinary
  | ArrayPush
  | Assert
  | LogOr
  | LogAnd

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private LoopDef = { Break: Label; Continue: Label }

let private newPlace local path : MPlace = { Local = local; Path = path }

let private localPlace local : MPlace = { Local = local; Path = Array.empty }

let private makeProjection part (place: MPlace) =
  { place with Path = Array.append place.Path [| part |] }

// -----------------------------------------------
// State
// -----------------------------------------------

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private MgState =
  { mutable Locals: Map<Symbol, LocalDef>
    Loop: LoopDef option
    FnOpt: FnDef option
    ArrayMemo: Dictionary<MTy, Symbol>
    Arrays: ResizeArray<ArrayDef>
    Records: Map<Symbol, RecordDef>
    Stmts: ResizeArray<MStmt>
    mutable TerminatorOpt: MTerminator option }

let private initialState: MgState =
  { Locals = Map.empty
    ArrayMemo = Dictionary()
    Arrays = ResizeArray()
    Records = Map.empty
    Loop = None
    FnOpt = None
    Stmts = ResizeArray()
    TerminatorOpt = None }

let private cloneState (state: MgState) : MgState = { state with Locals = state.Locals }

let private addLocal (state: MgState) (def: LocalDef) =
  let index = state.Locals |> Map.count
  let symbol = newSymbol "_" index def.Name
  state.Locals <- state.Locals |> Map.add symbol def
  symbol

// -----------------------------------------------
// Types
// -----------------------------------------------

let private internArrayTy (state: MgState) itemTy =
  match state.ArrayMemo.TryGetValue(itemTy) with
  | true, it -> it

  | false, _ ->
    let symbol = newSymbol "a" state.Arrays.Count "array"
    let def: ArrayDef = { ItemTy = itemTy }
    state.Arrays.Add(def)
    symbol

let private internTy (state: MgState) ty =
  match ty with
  | Ty.Void -> MTy.Void
  | Ty.Bool -> MTy.Bool
  | Ty.Int -> MTy.Int
  | Ty.String -> MTy.String

  | Ty.Record record -> MTy.Record record

  | Ty.Array itemTy ->
    let itemTy = internTy state itemTy
    internArrayTy state itemTy |> MTy.Array

let private addStmt (state: MgState) stmt = state.Stmts.Add(stmt)

let private setTerminator (state: MgState) terminator = state.TerminatorOpt <- Some terminator

// -----------------------------------------------
// Places
// -----------------------------------------------

let private genAsPlace (state: MgState) (place: Place) =
  match place with
  | Place.Local local -> localPlace local

  | Place.Index (lhs, index) ->
    let place = genAsPlace state lhs
    let arrayDef = failwith "todo"
    let index = genAsRval state index

    makeProjection (Part.Index(index, arrayDef)) place

  | Place.Field (lhs, field) ->
    let place = genAsPlace state lhs
    failwith "todo"

let private genAsRval (state: MgState) (expr: Expr) =
  match expr with
  | Expr.Read place -> genAsPlace state place |> MRval.Read

  | Expr.Void -> MRval.Void
  | Expr.Bool value -> MRval.Bool value
  | Expr.Int value -> MRval.Int value
  | Expr.String value -> MRval.String value

  | Expr.Array (itemTy, items) ->
    let arrayDef = internArrayTy state (internTy state itemTy)

    let local =
      let localDef: LocalDef =
        { Name = "array"
          Ty = MTy.Array arrayDef }

      addLocal state localDef

    let items =
      items
      |> List.toArray
      |> Array.map (fun item -> genAsRval state item)

    addStmt state (MStmt.InitArray(localPlace local, items, arrayDef))

    MRval.Read(localPlace local)

  | Expr.Record (record, fields) ->
    let local =
      let localDef: LocalDef =
        { Name = record.Name
          Ty = MTy.Record record }

      addLocal state localDef

    let fields =
      fields
      |> List.toArray
      |> Array.map (fun field -> genAsRval state field)

    addStmt state (MStmt.InitRecord(localPlace local, fields, record))

    MRval.Read(localPlace local)

  | Expr.Call (callable, args) ->
    match callable with
    | Callable.Fn fn -> failwith "todo"

    | Callable.LogOr -> failwith "Not Implemented"
    | Callable.LogAnd -> failwith "Not Implemented"

    | Callable.ArrayPush -> failwith "Not Implemented"
    | Callable.Assert -> failwith "Not Implemented"

  | Expr.Unary (unary, arg) -> failwith "Not Implemented"
  | Expr.Binary (binary, lhs, rhs) -> failwith "Not Implemented"

// -----------------------------------------------
// Statements
// -----------------------------------------------

let private genStmt (state: MgState) (stmt: Stmt) =
  match stmt with
  | Stmt.Do expr -> genAsRval state expr |> ignore

  | Stmt.Assign (place, value) ->
    let place = genAsPlace state place
    let value = genAsRval state value
    addStmt state (MStmt.Assign(place, value))

  | Stmt.Break -> failwith "todo"
  | Stmt.Continue -> failwith "todo"

  | Stmt.Return result ->
    assert (Option.isSome state.FnOpt)

    let dest = newSymbol "_" 0 "__return"

    let result = genAsRval state result
    addStmt state (MStmt.Assign(localPlace dest, result))
    setTerminator state MTerminator.Return

  | Stmt.Block block ->
    for stmt in block.Stmts do
      genStmt state stmt

  | Stmt.If (cond, body, alt) ->
    let cond = genAsRval state cond

    // begin branch
    // genStmt state body
    // genStmt state alt
    failwith "todo"

  | Stmt.Loop body ->
    // genStmt state body
    failwith "todo"

// -----------------------------------------------
// Declarations
// -----------------------------------------------

let private genDecl (state: MgState) (decl: Decl) =
  match decl with
  | Decl.Block (locals, block) ->
    // let innerState = cloneState state

    // for stmt in block.Stmts do
    //   try
    //     checkStmt innerState stmt
    //   with
    //   | _ ->
    //     eprintfn "In statement: %A" stmt
    //     reraise ()
    failwith "todo"

  | Decl.Fn (symbol, paramList, resultTy, locals, body) ->
    // let paramTys = paramList |> List.map snd

    // state.ValueEnv <-
    //   state.ValueEnv
    //   |> Map.add name (ValueDef.Fn(paramTys, resultTy))

    // let innerState = cloneState state

    // for paramName, paramTy in paramList do
    //   innerState.ValueEnv <-
    //     innerState.ValueEnv
    //     |> Map.add paramName (ValueDef.Local paramTy)

    // innerState.ValueEnv <-
    //   innerState.ValueEnv
    //   |> Map.add "__return" (ValueDef.Local resultTy)

    // try
    //   checkStmt innerState body
    // with
    // | _ ->
    //   eprintfn "In fn: %s" name
    //   reraise ()
    failwith "todo"

  | Decl.RecordTy _ -> ()

// -----------------------------------------------
// Interface
// -----------------------------------------------

let genMir (decls: Decl list) =
  let state = initialState
  let records = Dictionary()

  for decl in decls do
    match decl with
    | Decl.RecordTy (record, _) ->
      let def: RecordDef =
        { Name = record.Name
          Fields = Array.empty }

      records.Add(record, def)

    | _ -> ()

  for decl in decls do
    match decl with
    | Decl.RecordTy (record, fields) ->
      let def: RecordDef =
        { Name = record.Name
          Fields =
            fields
            |> Array.map (fun (name, ty) ->
              ({ Name = name.Name
                 Ty = internTy state ty }: FieldDef)) }

      records.[record] <- def

    | _ -> ()

  for decl in decls do
    genDecl state decl

  failwith "todo"
