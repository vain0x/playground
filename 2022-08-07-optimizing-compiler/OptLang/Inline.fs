module rec OptLang.Inline

open OptLang.Mir
open OptLang.Symbol

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private CollectState = { mutable FnFreq: Map<Symbol, int> }

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

let private localPlace local : MPlace = { Local = local; Path = Array.empty }

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

  // eprintfn
  //   "unusedFns: [%s]"
  //   (unusedFns.ToArray()
  //    |> Array.map string
  //    |> String.concat "; ")

  // eprintfn
  //   "singleUseFns: [%s]"
  //   (singleUseFns.ToArray()
  //    |> Array.map string
  //    |> String.concat "; ")

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
      | MStmt.Call (place, MCallable.Fn fn, args) when inlinedFns |> Map.containsKey fn -> Some(i, place, fn, args)
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
  // - `return` はターゲットブロックへのジャンプに置き換える
  let subst dest (blocks: MBlockDef array) =
    let substTerminator terminator =
      match terminator with
      | MTerminator.Return -> MTerminator.Goto dest

      | MTerminator.Unreachable
      | MTerminator.Goto _
      | MTerminator.If _ -> terminator

    blocks
    |> Array.map (fun blockDef -> ({ blockDef with Terminator = substTerminator blockDef.Terminator }: MBlockDef))

  let inlineBody (mir: MProgram) (callerDef: MFnDef) =
    match callerDef.Blocks
          |> tryPickIndex (fun block blockDef ->
            match find 0 blockDef.Stmts with
            | Some (stmtId, place, fn, args) -> Some(block, stmtId, place, fn, args)
            | None -> None)
      with
    | Some (block, stmtId, place, fn, args) ->
      eprintfn "inline %A" fn

      let calleeDef = mir.Fns |> lookup fn
      eprintfn "calleeDef = %A" calleeDef
      eprintfn "block: %d" block

      let blockDef = callerDef.Blocks.[block]
      let localMap = callerDef.Locals
      let callerLocalCount = callerDef.Locals.Count
      let callerBlockCount = callerDef.Blocks.Length

      // 呼び出される関数のエントリーブロック
      let entryBlock = newSymbol "B" callerBlockCount calleeDef.Name

      // 呼び出しから戻る先のブロック
      let restBlock =
        newSymbol "B" (callerBlockCount + calleeDef.Blocks.Length) "after_inline"

      // 呼び出される関数の返り値を受け取るローカル
      let result = newSymbol "_" callerLocalCount "__return"

      // 呼び出される関数のローカル、ブロックを呼び出し側に埋め込めるように調整する
      //
      // 番号が被らないようにずらす。`return` をジャンプに置き換える
      let calleeLocals =
        calleeDef.Locals
        |> Map.toArray
        |> Array.map (fun (local, localDef) -> shiftLocalBy callerLocalCount local, localDef)

      let calleeBlocks =
        shift callerLocalCount callerBlockCount calleeDef.Blocks
        |> subst restBlock

      // 呼び出される関数のパラメータに引数を設定する文を生成する
      let prologue =
        Array.zip calleeDef.Params args
        |> Array.map (fun ((param, _), arg) -> MStmt.Assign(localPlace (shiftLocalBy callerLocalCount param), arg))

      // 呼び出される関数の結果を所定のローカルに代入する
      let epilogue = [| MStmt.Assign(place, MRval.Read(localPlace result)) |]

      // 呼び出し側のブロックを分割する。
      // 呼び出すブロックはインラインされた関数のエントリーブロックへのジャンプで終了する。
      // インラインされた関数の `return` から、後半のブロックへジャンプしてくる
      let callerBlockDef: MBlockDef =
        { blockDef with
            Stmts = Array.append blockDef.Stmts.[0 .. stmtId - 1] prologue
            Terminator = MTerminator.Goto entryBlock }

      let restBlockDef: MBlockDef =
        { Stmts = Array.append epilogue blockDef.Stmts.[stmtId + 1 ..]
          Terminator = blockDef.Terminator }

      { callerDef with
          Locals = mapAppend calleeLocals localMap
          Blocks =
            [| yield!
                 callerDef.Blocks
                 |> Array.mapi (fun i blockDef ->
                   if i = block then
                     callerBlockDef
                   else
                     blockDef)

               yield! calleeBlocks
               yield restBlockDef |] }
      |> Some

    | None -> None

  // ワークリスト処理:

  let mutable mir = mir

  let rec workLoop workList =
    match workList with
    | fn :: workList ->
      let fnDef = mir.Fns.[fn]

      match inlineBody mir fnDef with
      | Some fnDef2 ->
        assert (System.Object.ReferenceEquals(mir.Fns.[fn], fnDef))
        mir <- { mir with Fns = mir.Fns |> Map.add fn fnDef2 }
        workLoop (fn :: workList)

      | None -> workLoop workList

    | [] -> ()

  workLoop (List.ofSeq mir.Fns.Keys)
  mir
