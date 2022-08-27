module rec OptLang.MirGen

open OptLang.Mir
open OptLang.Symbol
open OptLang.Tir
open System.Collections.Generic

let inline private unreachable () = failwith "unreachable"

let inline private withContext (label: string) (data: obj) ([<InlineIfLambda>] action) =
  try
    action ()
  with
  | _ ->
    eprintfn "In %s: %A" label data
    reraise ()

let inline private unwrap opt =
  match opt with
  | Some it -> it
  | None -> unreachable ()

let private lookup key map =
  match map |> Map.tryFind key with
  | Some it -> it
  | None -> failwithf "unreachable. Missing key: %A" key

let private dictToMap (dict: Dictionary<_, _>) =
  dict
  |> Seq.map (fun (KeyValue (key, value)) -> key, value)
  |> Map.ofSeq

// -----------------------------------------------
// MIR Helpers
// -----------------------------------------------

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private LoopDef =
  { Break: Symbol
    Continue: Symbol
    mutable MightBreak: bool }

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
    mutable Stmts: ResizeArray<MStmt>
    mutable Terminator: MTerminator

    // Body-local data
    LoopOpt: LoopDef option
    mutable Locals: Map<Symbol, MLocalDef>
    ResultTy: MTy
    Blocks: ResizeArray<MBlockDef>

    // Global data
    Bodies: ResizeArray<MBodyDef>
    mutable Fns: Map<Symbol, MFnDef>
    ArrayMemo: Dictionary<MTy, Symbol>
    Arrays: ResizeArray<MArrayDef>
    Records: Map<Symbol, RecordDef> }

let private initialState () : MgState =
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
    Terminator = MTerminator.Unreachable
    Blocks = ResizeArray() }

let private cloneState (state: MgState) : MgState = { state with Locals = state.Locals }

let private addLocal (state: MgState) (def: MLocalDef) =
  let index = state.Locals |> Map.count
  let symbol = newSymbol "_" index def.Name
  state.Locals <- state.Locals |> Map.add symbol def
  symbol

let private placeToTy (state: MgState) (place: MPlace) =
  let mutable ty = (state.Locals |> lookup place.Local).Ty

  for part in place.Path do
    match part with
    | MPart.Index (_, array) ->
      let arrayDef = state.Arrays.[array.Index]
      ty <- arrayDef.ItemTy

    | MPart.Field (index, record) ->
      let recordDef = state.Records |> lookup record
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
    let def: MArrayDef = { ItemTy = itemTy }
    state.Arrays.Add(def)
    symbol

let private internTy (state: MgState) ty =
  match ty with
  | TTy.Void -> MTy.Void
  | TTy.Bool -> MTy.Bool
  | TTy.Int -> MTy.Int
  | TTy.String -> MTy.String
  | TTy.Record record -> MTy.Record record

  | TTy.Array itemTy ->
    let itemTy = internTy state itemTy
    internArrayTy state itemTy |> MTy.Array

let private addStmt (state: MgState) stmt = state.Stmts.Add(stmt)

let private EmptyBlockDef: MBlockDef =
  { Stmts = Array.empty
    Terminator = MTerminator.Unreachable }

let private addBlock (state: MgState) name =
  let index = state.Blocks.Count
  let symbol = newSymbol "B" index name
  state.Blocks.Add(EmptyBlockDef)
  symbol

// derive equality
[<NoComparison>]
type private Flow =
  | Step
  | Break
  | Continue
  | Return

/// 分岐から合流するときのフローを計算する
///
/// - 各枝のフローの種類はここでは気にしなくてよい
///   (フローは枝のブロックを閉じる際のターミネータの選択で使用済みであるため)
///   すべての枝が発散するなら、発散することを示すためにReturnを返す
let private mergeFlow f1 f2 =
  match f1, f2 with
  | Step, _
  | _, Step -> Step

  | _ -> Return

/// 新しいブロックの生成を開始する
///
/// - `terminator`: ブロックの終端に到達したときに使う
/// - コードを生成した後、ブロックを閉じること (finishBlock)
let private startBlock (state: MgState) (block: Symbol) terminator =
  assert (System.Object.ReferenceEquals(state.Blocks.[block.Index], EmptyBlockDef))
  assert (Option.isNone state.BlockOpt)

  { state with
      BlockOpt = Some block
      Stmts = ResizeArray()
      Terminator = terminator }

let private finishBlock (state: MgState) flow =
  let block = unwrap state.BlockOpt

  let terminator =
    match flow with
    | Step -> state.Terminator
    | Break -> MTerminator.Goto (unwrap state.LoopOpt).Break
    | Continue -> MTerminator.Goto (unwrap state.LoopOpt).Continue
    | Return -> MTerminator.Return

  let blockDef: MBlockDef =
    { Stmts = state.Stmts.ToArray()
      Terminator = terminator }

  assert (System.Object.ReferenceEquals(state.Blocks.[block.Index], EmptyBlockDef))
  state.Blocks.[block.Index] <- blockDef

  state.BlockOpt <- None
  state.Stmts.Clear()
  state.Terminator <- MTerminator.Unreachable

/// 分岐を開始する
///
/// - これを呼ぶ前に現在のブロックを閉じておくこと (finishBlockを使う)
/// - これの後に分岐先のブロックを生成し、その後、新しいブロックを開くこと (joinを使う)
let private split (state: MgState) terminator next =
  assert (Option.isSome state.BlockOpt)

  let t = state.Terminator
  state.Terminator <- terminator
  finishBlock state Step

  next, t

/// 分岐から合流する
let private join (state: MgState) next =
  let nextBlock, terminator = next
  let s = startBlock state nextBlock terminator
  state.BlockOpt <- s.BlockOpt
  state.Stmts <- s.Stmts
  state.Terminator <- s.Terminator

// -----------------------------------------------
// Places
// -----------------------------------------------

let private genAsPlace (state: MgState) (place: TPlace) =
  match place with
  | TPlace.Local local -> localPlace local

  | TPlace.Index (lhs, index) ->
    let place = genAsPlace state lhs

    let array =
      match placeToTy state place with
      | MTy.Array it -> it
      | _ -> unreachable ()

    let index = genAsRval state index
    makeProjection (MPart.Index(index, array)) place

  | TPlace.Field (lhs, field) ->
    let place = genAsPlace state lhs

    let record =
      match placeToTy state place with
      | MTy.Record it -> it
      | _ -> unreachable ()

    makeProjection (MPart.Field(field.Index, record)) place

let private genAsRval (state: MgState) (expr: TExpr) =
  match expr with
  | TExpr.Read place -> genAsPlace state place |> MRval.Read

  | TExpr.Void -> MRval.Void
  | TExpr.Bool value -> MRval.Bool value
  | TExpr.Int value -> MRval.Int value
  | TExpr.String value -> MRval.String value

  | TExpr.Array (itemTy, items) ->
    let array = internArrayTy state (internTy state itemTy)

    let items =
      items
      |> List.toArray
      |> Array.map (fun item -> genAsRval state item)

    MRval.Array(items, array)

  | TExpr.Record (record, fields) ->
    let fields =
      fields
      |> List.toArray
      |> Array.map (fun field -> genAsRval state field)

    MRval.Record(fields, record)

  | TExpr.Call (callable, args) ->
    match callable with
    | TCallable.Fn fn ->
      let fnDef = state.Fns |> lookup fn

      let result =
        match fnDef.ResultTy with
        | MTy.Void -> newSymbol "_" 0 "__void"
        | _ ->
          let resultDef: MLocalDef = { Name = fn.Name; Ty = fnDef.ResultTy }
          addLocal state resultDef

      let args =
        args
        |> List.toArray
        |> Array.map (fun arg -> genAsRval state arg)

      addStmt state (MStmt.Call(localPlace result, MCallable.Fn fn, args))

      match fnDef.ResultTy with
      | MTy.Void -> MRval.Void
      | _ -> MRval.Read(localPlace result)

    | TCallable.LogOr ->
      let cond, alt =
        match args with
        | [ lhs; rhs ] -> lhs, rhs
        | _ -> unreachable ()

      let cond = genAsRval state cond

      let dest =
        let localDef: MLocalDef = { Name = "log_or"; Ty = MTy.Bool }
        addLocal state localDef

      let bodyBlock = addBlock state "then"
      let altBlock = addBlock state "else"
      let nextBlock = addBlock state "endif"
      let next = split state (MTerminator.If(cond, bodyBlock, altBlock)) nextBlock

      do
        let state = startBlock state bodyBlock (MTerminator.Goto nextBlock)
        addStmt state (MStmt.Assign(localPlace dest, MRval.Bool true))
        finishBlock state Step

      do
        let state = startBlock state altBlock (MTerminator.Goto nextBlock)
        addStmt state (MStmt.Assign(localPlace dest, genAsRval state alt))
        finishBlock state Step

      join state next
      MRval.Read(localPlace dest)

    | TCallable.LogAnd ->
      let cond, body =
        match args with
        | [ lhs; rhs ] -> lhs, rhs
        | _ -> unreachable ()

      let cond = genAsRval state cond

      let dest =
        let localDef: MLocalDef = { Name = "log_and"; Ty = MTy.Bool }
        addLocal state localDef

      let bodyBlock = addBlock state "then"
      let altBlock = addBlock state "else"
      let nextBlock = addBlock state "endif"
      let next = split state (MTerminator.If(cond, bodyBlock, altBlock)) nextBlock

      do
        let state = startBlock state bodyBlock (MTerminator.Goto nextBlock)
        addStmt state (MStmt.Assign(localPlace dest, genAsRval state body))
        finishBlock state Step

      do
        let state = startBlock state altBlock (MTerminator.Goto nextBlock)
        addStmt state (MStmt.Assign(localPlace dest, MRval.Bool false))
        finishBlock state Step

      join state next
      MRval.Read(localPlace dest)

    | TCallable.ArrayPush ->
      let args =
        args
        |> List.toArray
        |> Array.map (fun arg -> genAsRval state arg)

      let result = newSymbol "_" 0 "__void"

      addStmt state (MStmt.Call(localPlace result, MCallable.ArrayPush, args))
      MRval.Void

    | TCallable.Assert ->
      let args =
        args
        |> List.toArray
        |> Array.map (fun arg -> genAsRval state arg)

      let result = newSymbol "_" 0 "__void"

      addStmt state (MStmt.Call(localPlace result, MCallable.Assert, args))
      MRval.Void

  | TExpr.Unary (unary, arg) ->
    let unary =
      match unary with
      | TUnary.Not -> MUnary.Not
      | TUnary.Minus -> MUnary.Minus
      | TUnary.ArrayLen -> MUnary.ArrayLen

    let arg = genAsRval state arg
    MRval.Unary(unary, arg)

  | TExpr.Binary (binary, lhs, rhs) ->
    let binary =
      match binary with
      | TBinary.Add -> MBinary.Add
      | TBinary.Subtract -> MBinary.Subtract
      | TBinary.Multiply -> MBinary.Multiply
      | TBinary.Divide -> MBinary.Divide
      | TBinary.Modulo -> MBinary.Modulo
      | TBinary.Equal -> MBinary.Equal
      | TBinary.NotEqual -> MBinary.NotEqual
      | TBinary.LessThan -> MBinary.LessThan
      | TBinary.LessEqual -> MBinary.LessEqual
      | TBinary.GreaterThan -> MBinary.GreaterThan
      | TBinary.GreaterEqual -> MBinary.GreaterEqual

    let lhs = genAsRval state lhs
    let rhs = genAsRval state rhs
    MRval.Binary(binary, lhs, rhs)

// -----------------------------------------------
// Statements
// -----------------------------------------------

let private genStmt (state: MgState) (stmt: TStmt) =
  match stmt with
  | TStmt.Do expr ->
    genAsRval state expr |> ignore
    Step

  | TStmt.Assign (place, value) ->
    let place = genAsPlace state place
    let value = genAsRval state value
    addStmt state (MStmt.Assign(place, value))
    Step

  | TStmt.Break ->
    assert (Option.isSome state.LoopOpt)
    (unwrap state.LoopOpt).MightBreak <- true
    Break

  | TStmt.Continue ->
    assert (Option.isSome state.LoopOpt)
    Continue

  | TStmt.Return result ->
    // assert (Option.isSome state.FnOpt)

    let dest = newSymbol "_" 0 "__return"
    let result = genAsRval state result
    addStmt state (MStmt.Assign(localPlace dest, result))
    Return

  | TStmt.Block block -> genStmtBlock state block.Stmts

  | TStmt.If (cond, body, alt) ->
    let cond = genAsRval state cond

    let bodyBlock = addBlock state "then"
    let altBlock = addBlock state "else"
    let nextBlock = addBlock state "endif"
    let next = split state (MTerminator.If(cond, bodyBlock, altBlock)) nextBlock

    let bodyFlow =
      let state = startBlock state bodyBlock (MTerminator.Goto nextBlock)
      let flow = genStmt state body
      finishBlock state flow
      flow

    let altFlow =
      let state = startBlock state altBlock (MTerminator.Goto nextBlock)
      let flow = genStmt state alt
      finishBlock state flow
      flow

    join state next
    mergeFlow bodyFlow altFlow

  | TStmt.Loop body ->
    let bodyBlock = addBlock state "loop_body"
    let nextBlock = addBlock state "loop_next"
    let next = split state (MTerminator.Goto bodyBlock) nextBlock

    let flow =
      let state =
        let s = startBlock state bodyBlock (MTerminator.Goto bodyBlock)

        { s with
            LoopOpt =
              Some(
                { Break = nextBlock
                  Continue = bodyBlock
                  MightBreak = false }
              ) }

      let flow =
        match genStmt state body with
        | Step -> Continue
        | it -> it

      finishBlock state flow

      if (unwrap state.LoopOpt).MightBreak then
        Step
      else
        // 発散を表す
        Return

    join state next
    flow

/// 複数の文をコード生成する
let private genStmtBlock (state: MgState) stmts =
  let rec go stmts =
    match stmts with
    | [] -> Step

    | stmt :: stmts ->
      match genStmt state stmt with
      | Step -> go stmts

      | it ->
        // 発散するので、残りの文は到達できない
        if stmts |> List.isEmpty |> not then
          eprintfn "unreachable: %A" stmts

          for stmt in stmts do
            genStmt state stmt |> ignore

        it

  go stmts

// -----------------------------------------------
// Declarations
// -----------------------------------------------

let private genDecl (state: MgState) (decl: TDecl) =
  withContext "declaration" decl (fun () ->
    match decl with
    | TDecl.Block (locals, block) ->
      assert (Option.isNone state.BlockOpt)
      assert (state.Stmts.Count = 0)

      let locals =
        locals
        |> List.map (fun (local, ty) ->
          local,
          ({ Name = local.Name
             Ty = internTy state ty }: MLocalDef))

      let state =
        { state with
            LoopOpt = None
            ResultTy = MTy.Void
            Locals = Map.ofList locals
            Blocks = ResizeArray() }

      let entryBlock = addBlock state "entry"

      let innerState =
        let state = startBlock state entryBlock MTerminator.Return

        let flow =
          match genStmtBlock state block.Stmts with
          | Step -> Return
          | it -> it

        finishBlock state flow
        state

      let bodyDef: MBodyDef =
        { Locals = innerState.Locals
          Blocks = state.Blocks.ToArray() }

      state.Bodies.Add(bodyDef)

    | TDecl.Fn (fn, _, _, _, body) ->
      assert (Option.isNone state.BlockOpt)
      assert (state.Stmts.Count = 0)

      let fnDef = state.Fns |> lookup fn
      assert (fnDef.Blocks |> Array.isEmpty)

      let innerState =
        let state =
          { state with
              LoopOpt = None
              ResultTy = fnDef.ResultTy
              Locals = fnDef.Locals
              Blocks = ResizeArray() }

        let entryBlock = addBlock state "entry"

        let terminator =
          match fnDef.ResultTy with
          | MTy.Void -> MTerminator.Return
          | _ -> MTerminator.Unreachable

        let state = startBlock state entryBlock terminator
        let flow = genStmt state body
        finishBlock state flow
        state

      let fnDef =
        { fnDef with
            Locals = innerState.Locals
            Blocks = innerState.Blocks.ToArray() }

      state.Fns <- state.Fns |> Map.add fn fnDef

    | TDecl.RecordTy _ -> ())

// -----------------------------------------------
// Interface
// -----------------------------------------------

let genMir (decls: TDecl list) =
  let state = initialState ()
  let fns = Dictionary()
  let records = Dictionary()

  for decl in decls do
    match decl with
    | TDecl.Fn (fn, paramList, resultTy, locals, _) ->
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
             Ty = internTy state ty }: MLocalDef))
        |> Map.ofList

      let fnDef: MFnDef =
        { Name = fn.Name
          Params = paramList
          ResultTy = resultTy
          Locals = locals
          Blocks = Array.empty }

      fns.Add(fn, fnDef)

    | TDecl.RecordTy (record, fields) ->
      let def: RecordDef =
        { Name = record.Name
          Fields =
            fields
            |> Array.map (fun (name, ty) ->
              ({ Name = name.Name
                 Ty = internTy state ty }: MFieldDef)) }

      records.Add(record, def)

    | _ -> ()

  let state =
    { state with
        Fns = dictToMap fns
        Records = dictToMap records }

  for decl in decls do
    genDecl state decl

  ({ Bodies = state.Bodies.ToArray()
     Fns = state.Fns
     Records = state.Records
     Arrays = state.Arrays.ToArray() }: MProgram)
