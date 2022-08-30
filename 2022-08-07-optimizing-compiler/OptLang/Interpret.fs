module rec OptLang.Interpret

open System.Collections.Generic
open OptLang.Mir
open OptLang.Symbol

let inline private todo () = failwith "TODO"

[<RequireQualifiedAccess>]
type private Value =
  | Void
  | Bool of bool
  | Int of int
  | String of string
  | Array of ResizeArray<Value>
  | Record of Value array

[<RequireQualifiedAccess>]
type private Env =
  { Locals: Dictionary<Symbol, MTy>
    Store: Dictionary<Symbol, Value>
    Blocks: MBlockDef array }

let private newEnv () : Env =
  { Locals = Dictionary()
    Store = Dictionary()
    Blocks = Array.empty }

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

let interpret (mir: MProgram) =
  for bodyDef in mir.Bodies do
    let env = { newEnv () with Blocks = bodyDef.Blocks }
    defineLocals env bodyDef.Locals

    let mutable blockId = 0
    let mutable stmtId = 0

    let rec onRval (env: Env) rval =
      match rval with
      | MRval.Void -> Value.Void
      | MRval.Bool value -> Value.Bool value
      | MRval.Int value -> Value.Int value
      | MRval.String value -> Value.String value

      | MRval.Read place ->
        let mutable value = env.Store.[place.Local]

        for part in place.Path do
          match part with
          | MPart.Index (index, _) ->
            let array = value |> expectArray
            let index = onRval env index |> expectInt

            if uint array.Count < uint index then
              value <- array.[index]
            else
              failwithf "Index out of range (%d in %d)" index array.Count

          | MPart.Field (field, _) ->
            let record = value |> expectRecord
            value <- record.[field]

        value

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

        | _ -> todo ()

      | MRval.Record (fields, _) ->
        let fields = fields |> Array.map (onRval env)
        Value.Record fields

      | MRval.Array (items, _) ->
        let items = items |> Array.map (onRval env)
        Value.Array(ResizeArray(items))

    let rec go () =
      let stmts = bodyDef.Blocks.[blockId].Stmts

      if stmtId < stmts.Length then
        match stmts.[stmtId] with
        | MStmt.Assign (place, rval) -> todo ()
        | MStmt.Call (place, fn, args) -> todo ()

    go ()
