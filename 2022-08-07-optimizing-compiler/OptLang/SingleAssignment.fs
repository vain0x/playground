module rec OptLang.SingleAssignment

open System.Collections.Generic
open OptLang.Mir
open OptLang.Symbol

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private CollectState =
  { mutable LocalFreq: Dictionary<Symbol, int>
    mutable DefSet: HashSet<int * Symbol>
    mutable UseSet: HashSet<int * Symbol> }

module private CS =
  let init () : CollectState =
    { LocalFreq = Dictionary()
      DefSet = HashSet()
      UseSet = HashSet() }

  let onPlace (state: CollectState) block (place: MPlace) =
    do
      let local = place.Local

      if state.DefSet.Contains(block, local) |> not then
        if state.UseSet.Add(block, local) then
          eprintfn "use B%d:%A" block local

    for part in place.Path do
      match part with
      | MPart.Index (index, _) -> onRval state block index
      | MPart.Field _ -> ()

  let onRval (state: CollectState) block (rval: MRval) =
    match rval with
    | MRval.Void
    | MRval.Bool _
    | MRval.Int _
    | MRval.String _ -> ()

    | MRval.Read place -> onPlace state block place
    | MRval.Unary (_, arg) -> onRval state block arg
    | MRval.Binary (_, lhs, rhs) ->
      onRval state block lhs
      onRval state block rhs
    | MRval.Record (fields, _) -> onRvalList state block fields
    | MRval.Array (items, _) -> onRvalList state block items

  let onRvalList state block rvalList =
    for x in rvalList do
      onRval state block x

let performSingleAssignment (mir: MProgram) =
  for fnDef in mir.Fns.Values do
    let state: CollectState = CS.init ()

    do
      let block = 0

      for local, _ in fnDef.Params do
        eprintfn "def B%d:%A" block local
        state.DefSet.Add(block, local) |> ignore

    // ブロックごとの解析を行う
    // ブロックが定義するローカルを列挙する
    // ブロック内で定義より前に出現するローカルの使用を列挙する
    // ローカルの定義回数を数える
    for block, blockDef in Array.indexed fnDef.Blocks do
      for stmt in blockDef.Stmts do
        let dest =
          match stmt with
          | MStmt.Assign (dest, _)
          | MStmt.Call (dest, _, _) -> dest

        // rhs
        match stmt with
        | MStmt.Assign (_, rval) -> CS.onRval state block rval
        | MStmt.Call (_, _, args) -> CS.onRvalList state block args

        // lhs
        if Array.isEmpty dest.Path then
          let local = dest.Local

          match state.LocalFreq.TryGetValue(local) with
          | true, 1 -> state.LocalFreq.[local] <- 2
          | true, _ -> ()
          | _ -> state.LocalFreq.Add(local, 1)

          if state.DefSet.Add(block, local) then
            eprintfn "def B%d:%A" block local
        else
          CS.onPlace state block dest

      match blockDef.Terminator with
      | MTerminator.Unreachable
      | MTerminator.Goto _ -> ()

      | MTerminator.Return ->
        let local: MPlace =
          { Local = newSymbol "_" 0 "__return"
            Path = Array.empty }

        CS.onPlace state block local

      | MTerminator.If (cond, _, _) -> CS.onRval state block cond

    // ローカルの使用を伝播する
    // ブロックが定義していないローカルは、ターミネータでの移動先のブロックの使用に含まれている (入口生存である) ならブロックの使用に加える
    do
      let mutable redo = true

      while redo do
        redo <- false

        let onJump (dest: Symbol) =
          for b, local in state.UseSet do
            if
              b = dest.Index
              && not (state.DefSet.Contains(b, local))
            then
              if state.UseSet.Add(b, local) then
                eprintfn "use B%d %A" b local
                redo <- true

        for block in fnDef.Blocks.Length - 1 .. -1 .. 0 do
          let blockDef = fnDef.Blocks.[block]

          match blockDef.Terminator with
          | MTerminator.Unreachable
          | MTerminator.Return -> ()
          | MTerminator.Goto dest -> onJump dest

          | MTerminator.If (_, body, alt) ->
            onJump body
            onJump alt

  mir
