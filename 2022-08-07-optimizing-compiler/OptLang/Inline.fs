module rec OptLang.Inline

open OptLang.Mir
open OptLang.Symbol

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private CollectState = { mutable FnFreq: Map<Symbol, int> }

/// ワークリストの項目
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private W =
  | Body of index: int * MBodyDef
  | Fn of Symbol * MFnDef

let private tryPickIndex picker (array: _ array) =
  let rec go i =
    if i < array.Length then
      match picker i array.[i] with
      | (Some _) as opt -> opt
      | None -> go (i + 1)
    else
      None

  go 0

let private lookup key map =
  match map |> Map.tryFind key with
  | Some it -> it
  | None -> failwithf "unreachable. Missing key: %A" key

let private mapAppend entries map =
  entries
  |> Array.fold (fun map (local, localDef) -> map |> Map.add local localDef) map

let private shiftLocalBy localCount (local: Symbol) =
  newSymbol "_" (local.Index + localCount) local.Name

let performInlineExpansion (mir: MProgram) =
  // collect
  let state: CollectState = { FnFreq = Map.empty }

  let onBlock (state: CollectState) (block: MBlockDef) =
    for stmt in block.Stmts do
      match stmt with
      | MStmt.Call (_, MCallable.Fn fn, _) ->
        match state.FnFreq |> Map.tryFind fn with
        | None -> state.FnFreq <- state.FnFreq |> Map.add fn 1
        | Some 1 -> state.FnFreq <- state.FnFreq |> Map.add fn 2
        | _ -> ()
      | _ -> ()

  for bodyDef in mir.Bodies do
    for blockDef in bodyDef.Blocks do
      onBlock state blockDef

  for fnDef in mir.Fns.Values do
    for blockDef in fnDef.Blocks do
      onBlock state blockDef

  let unusedFns = ResizeArray()
  let singleUseFns = ResizeArray()

  for fn in mir.Fns.Keys do
    match state.FnFreq |> Map.tryFind fn with
    | None -> unusedFns.Add(fn)
    | Some 1 -> singleUseFns.Add(fn)
    | _ -> ()

  eprintfn
    "unusedFns: [%s]"
    (unusedFns.ToArray()
     |> Array.map string
     |> String.concat "; ")

  eprintfn
    "singleUseFns: [%s]"
    (singleUseFns.ToArray()
     |> Array.map string
     |> String.concat "; ")

  let inlinedFns =
    mir.Fns
    |> Map.filter (fun fn _ ->
      match state.FnFreq |> Map.tryFind fn with
      | Some 1 -> true
      | _ -> false)

  // transform

  // インライン展開可能な呼び出し文を探す
  let rec find i (stmts: _ array) =
    if i < stmts.Length then
      match stmts.[i] with
      | MStmt.Call (place, MCallable.Fn fn, args) -> Some(i, place, fn, args)
      | _ -> find (i + 1) stmts
    else
      None

  // ローカル変数とブロックの番号を付け替える
  let shift localCount blockCount (blocks: MBlockDef array) =
    let shiftLocal local = shiftLocalBy localCount local

    let shiftBlock (block: Symbol) =
      newSymbol block.Kind (block.Index + blockCount) block.Name

    let rec shiftPart (part: MPart) =
      match part with
      | MPart.Index (index, array) -> MPart.Index(shiftRval index, array)
      | MPart.Field _ -> part

    and shiftPlace (place: MPlace) =
      ({ Local = shiftLocal place.Local
         Path = place.Path |> Array.map shiftPart }: MPlace)

    and shiftRval (rval: MRval) =
      match rval with
      | MRval.Void
      | MRval.Bool _
      | MRval.Int _
      | MRval.String _ -> rval

      | MRval.Read place -> MRval.Read(shiftPlace place)
      | MRval.Unary (unary, arg) -> MRval.Unary(unary, shiftRval arg)
      | MRval.Binary (binary, lhs, rhs) -> MRval.Binary(binary, shiftRval lhs, shiftRval rhs)
      | MRval.Record (fields, record) -> MRval.Record(Array.map shiftRval fields, record)
      | MRval.Array (items, array) -> MRval.Array(Array.map shiftRval items, array)

    let shiftStmt stmt =
      match stmt with
      | MStmt.Assign (place, value) -> MStmt.Assign(shiftPlace place, shiftRval value)
      | MStmt.Call (place, callable, args) -> MStmt.Call(shiftPlace place, callable, args |> Array.map shiftRval)

    let shiftTerminator terminator =
      match terminator with
      | MTerminator.If (cond, body, alt) -> MTerminator.If(shiftRval cond, shiftBlock body, shiftBlock alt)
      | MTerminator.Goto block -> MTerminator.Goto(shiftBlock block)

      | MTerminator.Unreachable
      | MTerminator.Return -> terminator

    blocks
    |> Array.map (fun block ->
      let stmts = Array.map shiftStmt block.Stmts
      let terminator = shiftTerminator block.Terminator

      ({ Stmts = stmts
         Terminator = terminator }: MBlockDef))

  // インライン展開される関数の本体を呼び出し側に埋め込むために変形する
  //
  // - パラメータは引数 (place) に展開する
  // - `return` はターゲットブロックへのジャンプに置き換える
  let subst argMap dest (blocks: MBlockDef array) =
    let rec substPart (part: MPart) =
      match part with
      | MPart.Index (index, array) -> MPart.Index(substRval index, array)
      | MPart.Field _ -> part

    and substPlace (place: MPlace) : MPlace =
      let path = place.Path |> Array.map substPart

      match argMap |> Map.tryFind place.Local with
      | Some arg -> { arg with Path = Array.append arg.Path path }
      | None -> { place with Path = path }

    and substRval (rval: MRval) =
      match rval with
      | MRval.Void
      | MRval.Bool _
      | MRval.Int _
      | MRval.String _ -> rval

      | MRval.Read place -> MRval.Read(substPlace place)
      | MRval.Unary (unary, arg) -> MRval.Unary(unary, substRval arg)
      | MRval.Binary (binary, lhs, rhs) -> MRval.Binary(binary, substRval lhs, substRval rhs)
      | MRval.Record (fields, record) -> MRval.Record(Array.map substRval fields, record)
      | MRval.Array (items, array) -> MRval.Array(Array.map substRval items, array)

    let substStmt stmt =
      match stmt with
      | MStmt.Assign (place, value) -> MStmt.Assign(substPlace place, substRval value)
      | MStmt.Call (place, callable, args) -> MStmt.Call(substPlace place, callable, args |> Array.map substRval)

    let substTerminator terminator =
      match terminator with
      | MTerminator.If (cond, body, alt) -> MTerminator.If(substRval cond, body, alt)
      | MTerminator.Return -> MTerminator.Goto dest

      | MTerminator.Unreachable
      | MTerminator.Goto _ -> terminator

    blocks
    |> Array.map (fun block ->
      ({ Stmts = Array.map substStmt block.Stmts
         Terminator = substTerminator block.Terminator }: MBlockDef))

  let mutable mir = mir

  let rec workLoop workList =
    match workList with
    | W.Body (bodyId, bodyDef) :: workList ->
      match bodyDef.Blocks
            |> tryPickIndex (fun block blockDef ->
              match find 0 blockDef.Stmts with
              | Some (stmtId, place, fn, args) -> Some(block, stmtId, place, fn, args)
              | None -> None)
        with
      | Some (block, stmtId, place, fn, args) ->
        let fnDef = mir.Fns |> lookup fn
        let blockDef = bodyDef.Blocks.[block]
        let localMap = bodyDef.Locals

        let localMap, argMap, assignmentAcc =
          let argMap = Map.ofList [ newSymbol "_" 0 "__return", place ]

          Array.zip fnDef.Params args
          |> Array.fold
               (fun (localMap, argMap, stmts) ((param, ty), arg) ->
                 match arg with
                 | MRval.Read place ->
                   let argMap = argMap |> Map.add param place
                   localMap, argMap, stmts

                 | _ ->
                   let localDef: MLocalDef = { Name = param.Name; Ty = ty }
                   let local = newSymbol "_" (Map.count localMap) param.Name

                   let localMap = localMap |> Map.add local localDef

                   let place: MPlace = { Local = local; Path = Array.empty }
                   let stmts = MStmt.Assign(place, arg) :: stmts

                   let argMap = argMap |> Map.add param place

                   localMap, argMap, stmts)
               (localMap, argMap, [])

        let assignments =
          let a = assignmentAcc |> List.toArray
          System.Array.Reverse(a)
          a

        let localSize = localMap.Count - fnDef.Params.Length - 1
        let blockSize = bodyDef.Blocks.Length + 1

        let restBlock = newSymbol "B" bodyDef.Blocks.Length "after_inline"
        let entryBlock = newSymbol "B" (bodyDef.Blocks.Length + 1) fnDef.Name

        let restBlockDef: MBlockDef =
          { Stmts = blockDef.Stmts.[stmtId + 1 ..]
            Terminator = blockDef.Terminator }

        let fnLocals =
          fnDef.Locals
          |> Map.toArray
          |> Array.map (fun (local, localDef) -> shiftLocalBy localSize local, localDef)

        let fnBlocks =
          let argMap =
            argMap
            |> Map.toArray
            |> Array.map (fun (param, place) -> shiftLocalBy localSize param, place)
            |> Map.ofArray

          shift localSize blockSize fnDef.Blocks
          |> subst argMap restBlock

        assert (System.Object.ReferenceEquals(mir.Bodies.[bodyId], bodyDef))

        mir.Bodies.[bodyId] <-
          { mir.Bodies.[bodyId] with
              Locals = mapAppend fnLocals localMap
              Blocks =
                [| yield!
                     bodyDef.Blocks
                     |> Array.mapi (fun i blockDef ->
                       if i = block then
                         { blockDef with
                             Stmts = Array.append blockDef.Stmts.[0 .. stmtId - 1] assignments
                             Terminator = MTerminator.Goto entryBlock }
                       else
                         blockDef)

                   yield restBlockDef
                   yield! fnBlocks |] }

        workLoop (W.Body(bodyId, mir.Bodies.[bodyId]) :: workList)

      | None -> workLoop workList

    | W.Fn (fn, fnDef) :: workList ->
      // TODO
      workLoop workList

    | [] -> ()

  let workList =
    Array.append
      (mir.Bodies
       |> Array.mapi (fun i bodyDef -> W.Body(i, bodyDef)))
      (mir.Fns
       |> Map.toArray
       |> Array.map (fun (fn, fnDef) -> W.Fn(fn, fnDef)))
    |> Array.toList

  workLoop workList
  mir
