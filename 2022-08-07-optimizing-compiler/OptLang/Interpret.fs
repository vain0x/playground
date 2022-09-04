module rec OptLang.Interpret

open System.Collections.Generic
open OptLang.Mir
open OptLang.Symbol

module MirDump = OptLang.MirDump

let inline private todo () = failwith "TODO"

let inline private unreachable () = failwith "unreachable"

let private lookup key map =
  match map |> Map.tryFind key with
  | Some it -> it
  | None -> failwithf "unreachable. Missing key: %A" key

[<RequireQualifiedAccess>]
type private Value =
  | Void
  | Bool of bool
  | Int of int
  | String of string
  | Array of ResizeArray<Value>
  | Record of Value array

module private Value =
  let discriminant value =
    match value with
    | Value.Void -> 1
    | Value.Bool _ -> 2
    | Value.Int _ -> 3
    | Value.String _ -> 4
    | Value.Array _ -> 5
    | Value.Record _ -> 6

  let equal lhs rhs =
    match lhs, rhs with
    | Value.Void, Value.Void -> lhs = rhs
    | Value.Bool lhs, Value.Bool rhs -> lhs = rhs
    | Value.Int lhs, Value.Int rhs -> lhs = rhs
    | Value.String lhs, Value.String rhs -> lhs = rhs

    | Value.Array lhs, Value.Array rhs ->
      let rec go i =
        i = lhs.Count
        || (equal lhs.[i] rhs.[i] && go (i + 1))

      System.Object.ReferenceEquals(lhs, rhs)
      || lhs.Count = rhs.Count && go 0

    | Value.Record lhs, Value.Record rhs ->
      let rec go i =
        i = lhs.Length
        || (equal lhs.[i] rhs.[i] && go (i + 1))

      System.Object.ReferenceEquals(lhs, rhs)
      || (lhs.Length = rhs.Length && go 0)

    | _ -> false

  let compare lhs rhs =
    match lhs, rhs with
    | Value.Void, Value.Void -> 0
    | Value.Bool lhs, Value.Bool rhs -> Operators.compare lhs rhs
    | Value.Int lhs, Value.Int rhs -> Operators.compare lhs rhs
    | Value.String lhs, Value.String rhs -> Operators.compare lhs rhs

    | Value.Array lhs, Value.Array rhs ->
      let rec go i =
        if i < lhs.Count && i < rhs.Count then
          let c = compare lhs.[i] rhs.[i]
          if c = 0 then go (i + 1) else c
        else
          Operators.compare lhs.Count rhs.Count

      if System.Object.ReferenceEquals(lhs, rhs) then
        0
      else
        go 0

    | Value.Record lhs, Value.Record rhs ->
      let rec go i =
        if i < lhs.Length && i < rhs.Length then
          let c = compare lhs.[i] rhs.[i]
          if c = 0 then go (i + 1) else c
        else
          Operators.compare lhs.Length rhs.Length

      if System.Object.ReferenceEquals(lhs, rhs) then
        0
      else
        go 0

    | _ -> Operators.compare (discriminant lhs) (discriminant rhs)

[<RequireQualifiedAccess>]
type private Env =
  { Locals: Dictionary<Symbol, MTy>
    Store: Dictionary<Symbol, Value> }

let private newEnv () : Env =
  { Locals = Dictionary()
    Store = Dictionary() }

let private defineLocals (env: Env) (locals: Map<Symbol, MLocalDef>) =
  for KeyValue (local, localDef) in locals do
    env.Locals.Add(local, localDef.Ty)

let inline private typeError expected actual =
  failwithf "Type error: Expected %A but was %A" expected actual

let private expectBool value =
  match value with
  | Value.Bool it -> it
  | _ -> typeError "bool" value

let private expectInt value =
  match value with
  | Value.Int it -> it
  | _ -> typeError "int" value

let private expectString value =
  match value with
  | Value.String it -> it
  | _ -> typeError "string" value

let private expectArray value =
  match value with
  | Value.Array it -> it
  | _ -> typeError "array" value

let private expectRecord value =
  match value with
  | Value.Record it -> it
  | _ -> typeError "record" value

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type private BodyLike =
  | B of MBodyDef
  | F of MFnDef

let interpret (mir: MProgram) =
  let rec onPlace (env: Env) (place: MPlace) count =
    assert
      (count = place.Path.Length
       || count = place.Path.Length - 1)

    let mutable parent = env.Store.[place.Local]

    for i in 0 .. count - 1 do
      match place.Path.[i] with
      | MPart.Index (rval, _) ->
        let array = expectArray parent
        let index = onRval env rval |> expectInt
        parent <- array.[index]

      | MPart.Field (index, _) ->
        let record = expectRecord parent
        parent <- record.[index]

    parent

  and onRval (env: Env) rval =
    match rval with
    | MRval.Void -> Value.Void
    | MRval.Bool value -> Value.Bool value
    | MRval.Int value -> Value.Int value
    | MRval.String value -> Value.String value

    | MRval.Read place -> onPlace env place place.Path.Length

    | MRval.Unary (unary, arg) ->
      let arg = onRval env arg

      match unary with
      | MUnary.Not -> arg |> expectBool |> not |> Value.Bool
      | MUnary.Minus -> arg |> expectInt |> (~-) |> Value.Int
      | MUnary.ArrayLen -> Value.Int (expectArray arg).Count

    | MRval.Binary (binary, lhs, rhs) ->
      let lhs = onRval env lhs
      let rhs = onRval env rhs

      match binary with
      | MBinary.Add ->
        match lhs with
        | Value.Int lhs -> Value.Int(lhs + expectInt rhs)
        | Value.String lhs -> Value.String(lhs + expectString rhs)
        | _ -> typeError "(int | string)" lhs

      | MBinary.Subtract -> Value.Int(expectInt lhs - expectInt rhs)
      | MBinary.Multiply -> Value.Int(expectInt lhs * expectInt rhs)
      | MBinary.Divide -> Value.Int(expectInt lhs / expectInt rhs)
      | MBinary.Modulo -> Value.Int(expectInt lhs % expectInt rhs)

      | MBinary.Equal -> Value.Bool(Value.equal lhs rhs)
      | MBinary.NotEqual -> Value.Bool(Value.equal lhs rhs |> not)
      | MBinary.LessThan -> Value.Bool(Value.compare lhs rhs < 0)
      | MBinary.LessEqual -> Value.Bool(Value.compare lhs rhs <= 0)
      | MBinary.GreaterThan -> Value.Bool(Value.compare lhs rhs > 0)
      | MBinary.GreaterEqual -> Value.Bool(Value.compare lhs rhs >= 0)

    | MRval.Record (fields, _) ->
      let fields = fields |> Array.map (onRval env)
      Value.Record fields

    | MRval.Array (items, _) ->
      let items = items |> Array.map (onRval env)
      Value.Array(ResizeArray(items))

  and assign (env: Env) (place: MPlace) value =
    if place.Path |> Array.isEmpty then
      eprintfn "! assign %A <- %A" place.Local value
      env.Store.[place.Local] <- value
    else
      let parent = onPlace env place (place.Path.Length - 1)

      match place.Path.[place.Path.Length - 1] with
      | MPart.Index (rval, _) ->
        let array = expectArray parent
        let index = onRval env rval |> expectInt
        eprintfn "! ?array(%d) <- %A" index value
        array.[index] <- value

      | MPart.Field (index, _) ->
        let record = expectRecord parent
        eprintfn "! ?record(%d) <- %A" index value
        record.[index] <- value

  and run (env: Env) (blocks: MBlockDef array) =
    let mutable blockId = 0
    let mutable stmtId = 0

    let rec go () =
      let stmts = blocks.[blockId].Stmts

      if stmtId < stmts.Length then
        eprintfn "+ %s" (MirDump.displayStmt mir stmts.[stmtId])

        match stmts.[stmtId] with
        | MStmt.Assign (place, rval) -> assign env place (onRval env rval)

        | MStmt.Call (place, fn, args) ->
          let args = Array.map (onRval env) args

          match fn with
          | MCallable.Fn fn ->
            let fnDef = mir.Fns |> lookup fn
            let result = newSymbol "_" 0 "__return"

            let innerEnv = newEnv ()

            defineLocals innerEnv fnDef.Locals

            if fnDef.ResultTy = MTy.Void then
              innerEnv.Store.[result] <- Value.Void

            for KeyValue (local, _) in fnDef.Locals do
              let i = local.Index

              if i <> 0 && i <= args.Length then
                innerEnv.Store.[local] <- args.[i - 1]

            try
              run innerEnv fnDef.Blocks
            with
            | _ ->
              eprintfn "In fn %A" fn
              reraise ()

            // eprintfn "innerEnv:"

            // for KeyValue (k, v) in innerEnv.Store do
            //   eprintfn "  %A -> %A" k v

            assign env place innerEnv.Store.[result]

          | MCallable.ArrayPush ->
            match args with
            | [| array; item |] ->
              let array = expectArray array
              array.Add(item)

            | _ -> unreachable ()

          | MCallable.Assert ->
            if expectBool args.[0] |> not then
              failwithf "ERROR: Assertion violation!"

        stmtId <- stmtId + 1
        go ()
      else
        eprintfn "+ %s" (MirDump.displayTerminator mir blocks.[blockId].Terminator)

        match blocks.[blockId].Terminator with
        | MTerminator.Goto block ->
          blockId <- block.Index
          stmtId <- 0
          go ()

        | MTerminator.Unreachable -> failwith "ERROR: Unreachable"
        | MTerminator.Return -> ()

        | MTerminator.If (cond, body, alt) ->
          if onRval env cond |> expectBool then
            blockId <- body.Index
          else
            blockId <- alt.Index

          stmtId <- 0
          go ()

    go ()

  for bodyDef in mir.Bodies do
    let env = newEnv ()
    defineLocals env bodyDef.Locals
    run env bodyDef.Blocks
