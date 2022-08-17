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

let private dictToMap (dict: Dictionary<_, _>) =
  dict
  |> Seq.map (fun (KeyValue (key, value)) -> key, value)
  |> Map.ofSeq

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
  { // Block-local data
    mutable BlockOpt: Symbol option
    Stmts: ResizeArray<MStmt>
    mutable TerminatorOpt: MTerminator option

    // Body-local data
    LoopOpt: LoopDef option
    mutable Locals: Map<Symbol, LocalDef>
    ResultTy: MTy
    Blocks: ResizeArray<BlockDef>

    // Global data
    Bodies: ResizeArray<Map<Symbol, LocalDef> * BlockDef array>
    mutable Fns: Map<Symbol, FnDef>
    ArrayMemo: Dictionary<MTy, Symbol>
    Arrays: ResizeArray<ArrayDef>
    Records: Map<Symbol, RecordDef> }

let private initialState: MgState =
  { BlockOpt = None
    Locals = Map.empty
    Bodies = ResizeArray()
    Fns = Map.empty
    ArrayMemo = Dictionary()
    Arrays = ResizeArray()
    Records = Map.empty
    LoopOpt = None
    ResultTy = MTy.Void
    Stmts = ResizeArray()
    TerminatorOpt = None
    Blocks = ResizeArray() }

let private cloneState (state: MgState) : MgState = { state with Locals = state.Locals }

let private addLocal (state: MgState) (def: LocalDef) =
  let index = state.Locals |> Map.count
  let symbol = newSymbol "_" index def.Name
  state.Locals <- state.Locals |> Map.add symbol def
  symbol

let private placeToTy (state: MgState) (place: MPlace) =
  let mutable ty = (state.Locals |> Map.find place.Local).Ty

  for part in place.Path do
    match part with
    | Part.Index (_, array) ->
      let arrayDef = state.Arrays.[array.Index]
      ty <- arrayDef.ItemTy

    | Part.Field (index, record) ->
      let recordDef = state.Records |> Map.find record
      ty <- recordDef.Fields.[index].Ty

  ty

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

let private EmptyBlockDef: BlockDef =
  { Stmts = Array.empty
    Terminator = MTerminator.Unreachable }

let private addBlock (state: MgState) name =
  let index = state.Blocks.Count
  let symbol = newSymbol "B" index name
  state.Blocks.Add(EmptyBlockDef)
  symbol

let private resolveBlock (state: MgState) (block: Symbol) =
  assert (System.Object.ReferenceEquals(state.Blocks.[block.Index], EmptyBlockDef))

  let blockDef: BlockDef =
    { Stmts = state.Stmts.ToArray()
      Terminator =
        match state.TerminatorOpt with
        | Some it -> it
        | None -> failwithf "No terminator of %A" block }

  state.Blocks.[block.Index] <- blockDef

// -----------------------------------------------
// Places
// -----------------------------------------------

let private genAsPlace (state: MgState) (place: Place) =
  match place with
  | Place.Local local -> localPlace local

  | Place.Index (lhs, index) ->
    let place = genAsPlace state lhs

    let array =
      match placeToTy state place with
      | MTy.Array it -> it
      | _ -> unreachable ()

    let index = genAsRval state index
    makeProjection (Part.Index(index, array)) place

  | Place.Field (lhs, field) ->
    let place = genAsPlace state lhs

    let record =
      match placeToTy state place with
      | MTy.Record it -> it
      | _ -> unreachable ()

    makeProjection (Part.Field(field.Index, record)) place

let private genAsRval (state: MgState) (expr: Expr) =
  match expr with
  | Expr.Read place -> genAsPlace state place |> MRval.Read

  | Expr.Void -> MRval.Void
  | Expr.Bool value -> MRval.Bool value
  | Expr.Int value -> MRval.Int value
  | Expr.String value -> MRval.String value

  | Expr.Array (itemTy, items) ->
    let array = internArrayTy state (internTy state itemTy)

    let items =
      items
      |> List.toArray
      |> Array.map (fun item -> genAsRval state item)

    MRval.Array(items, array)

  | Expr.Record (record, fields) ->
    let fields =
      fields
      |> List.toArray
      |> Array.map (fun field -> genAsRval state field)

    MRval.Record(fields, record)

  | Expr.Call (callable, args) ->
    match callable with
    | Callable.Fn fn ->
      let fnDef = state.Fns |> Map.find fn

      let args =
        args
        |> List.toArray
        |> Array.map (fun arg -> genAsRval state arg)

      addStmt state (MStmt.Call(MCallable.Fn fn, args))

      match fnDef.ResultTy with
      | MTy.Void -> MRval.Void
      | _ ->
        let local = newSymbol "_" 0 "__return"
        MRval.Read(localPlace local)

    | Callable.LogOr -> failwith "todo"
    | Callable.LogAnd -> failwith "Not Implemented"

    | Callable.ArrayPush ->
      let args =
        args
        |> List.toArray
        |> Array.map (fun arg -> genAsRval state arg)

      addStmt state (MStmt.Call(MCallable.ArrayPush, args))
      MRval.Void

    | Callable.Assert -> failwith "Not Implemented"

  | Expr.Unary (unary, arg) ->
    let unary =
      match unary with
      | Unary.Not -> MUnary.Not
      | Unary.Minus -> MUnary.Minus
      | Unary.ArrayLen -> MUnary.ArrayLen

    let arg = genAsRval state arg
    MRval.Unary(unary, arg)

  | Expr.Binary (binary, lhs, rhs) ->
    let binary =
      match binary with
      | Binary.Add -> MBinary.Add
      | Binary.Subtract -> MBinary.Subtract
      | Binary.Multiply -> MBinary.Multiply
      | Binary.Divide -> MBinary.Divide
      | Binary.Modulo -> MBinary.Modulo
      | Binary.Equal -> MBinary.Equal
      | Binary.NotEqual -> MBinary.NotEqual
      | Binary.LessThan -> MBinary.LessThan
      | Binary.LessEqual -> MBinary.LessEqual
      | Binary.GreaterThan -> MBinary.GreaterThan
      | Binary.GreaterEqual -> MBinary.GreaterEqual

    let lhs = genAsRval state lhs
    let rhs = genAsRval state rhs
    MRval.Binary(binary, lhs, rhs)

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

  | Stmt.Break ->
    let label = (unwrap state.LoopOpt).Break
    setTerminator state (MTerminator.Goto label)

  | Stmt.Continue ->
    let label = (unwrap state.LoopOpt).Continue
    setTerminator state (MTerminator.Goto label)

  | Stmt.Return result ->
    // assert (Option.isSome state.FnOpt)

    let dest = newSymbol "_" 0 "__return"

    let result = genAsRval state result
    addStmt state (MStmt.Assign(localPlace dest, result))
    setTerminator state MTerminator.Return

  | Stmt.Block block ->
    for stmt in block.Stmts do
      genStmt state stmt

  | Stmt.If (cond, body, alt) ->
    let cond = genAsRval state cond

    let bodyBlock = addBlock state "then"
    let altBlock = addBlock state "else"
    let nextBlock = addBlock state "endif"
    setTerminator state (MTerminator.If(cond, bodyBlock, altBlock))
    state.BlockOpt <- Some nextBlock

    do
      let state =
        { state with
            Stmts = ResizeArray()
            TerminatorOpt = Some(MTerminator.Goto nextBlock) }

      genStmt state body
      resolveBlock state bodyBlock

    do
      let state =
        { state with
            Stmts = ResizeArray()
            TerminatorOpt = Some(MTerminator.Goto nextBlock) }

      genStmt state alt
      resolveBlock state altBlock

  | Stmt.Loop body ->
    let bodyBlock = addBlock state "loop_body"
    let nextBlock = addBlock state "loop_next"
    setTerminator state (MTerminator.Goto bodyBlock)
    state.BlockOpt <- Some nextBlock

    do
      let state =
        { state with
            LoopOpt =
              Some(
                { Break = nextBlock
                  Continue = bodyBlock }
              )
            Stmts = ResizeArray()
            TerminatorOpt = Some(MTerminator.Goto bodyBlock) }

      genStmt state body
      resolveBlock state bodyBlock

// -----------------------------------------------
// Declarations
// -----------------------------------------------

let private genDecl (state: MgState) (decl: Decl) =
  try
    match decl with
    | Decl.Block (locals, block) ->
      let locals =
        locals
        |> List.map (fun (local, ty) ->
          local,
          ({ Name = local.Name
             Ty = internTy state ty }: LocalDef))

      let state =
        { state with
            Stmts = ResizeArray()
            TerminatorOpt = Some MTerminator.Return

            LoopOpt = None
            ResultTy = MTy.Void
            Locals = Map.ofList locals
            Blocks = ResizeArray() }

      let entryBlock = addBlock state "entry"

      for stmt in block.Stmts do
        genStmt state stmt

      resolveBlock state entryBlock

      let bodyDef = state.Locals, state.Blocks.ToArray()
      state.Bodies.Add(bodyDef)

    | Decl.Fn (fn, _, _, _, body) ->
      let fnDef = state.Fns |> Map.find fn
      assert (fnDef.Blocks |> Array.isEmpty)

      let innerState =
        let state =
          { state with
              Stmts = ResizeArray()
              TerminatorOpt =
                Some(
                  match fnDef.ResultTy with
                  | MTy.Void -> MTerminator.Return
                  | _ -> MTerminator.Unreachable
                )

              LoopOpt = None
              Locals = fnDef.Locals
              ResultTy = fnDef.ResultTy
              Blocks = ResizeArray() }

        let entryBlock = addBlock state "entry"
        genStmt state body
        resolveBlock state entryBlock
        state

      let fnDef = { fnDef with Blocks = innerState.Blocks.ToArray() }
      state.Fns <- state.Fns |> Map.add fn fnDef

    | Decl.RecordTy _ -> ()
  with
  | _ ->
    eprintfn "In decl %A" decl
    reraise ()

// -----------------------------------------------
// Interface
// -----------------------------------------------

let genMir (decls: Decl list) =
  let state = initialState
  let fns = Dictionary()
  let records = Dictionary()

  for decl in decls do
    match decl with
    | Decl.Fn (fn, paramList, resultTy, locals, _) ->
      let paramList =
        paramList
        |> Array.ofList
        |> Array.map (fun (local, ty) -> local, internTy state ty)

      let resultTy = internTy state resultTy

      let locals =
        locals
        |> List.map (fun (local, ty) ->
          local,
          ({ Name = local.Name
             Ty = internTy state ty }: LocalDef))
        |> Map.ofList

      let fnDef: FnDef =
        { Name = fn.Name
          Params = paramList
          ResultTy = resultTy
          Locals = locals
          Blocks = Array.empty }

      fns.Add(fn, fnDef)

    | Decl.RecordTy (record, fields) ->
      let def: RecordDef =
        { Name = record.Name
          Fields =
            fields
            |> Array.map (fun (name, ty) ->
              ({ Name = name.Name
                 Ty = internTy state ty }: FieldDef)) }

      records.Add(record, def)

    | _ -> ()

  let state =
    { state with
        Fns = dictToMap fns
        Records = dictToMap records }

  for decl in decls do
    genDecl state decl

  failwith "todo"
