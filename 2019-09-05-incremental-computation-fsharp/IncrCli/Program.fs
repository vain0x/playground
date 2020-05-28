module Program

type Hash = int

type KeyHash = Hash

type QueryHash = Hash

type ResultHash = Hash

type ResultObj = obj

type Deps = (KeyHash * QueryHash * ResultHash) list

type Database =
  {
    Serial: int
    Cache: (KeyHash * QueryHash * ResultHash * ResultObj * Deps) list
    Stack: (KeyHash * QueryHash * ResultHash) list
  }

type Query<'K, 'T> =
  Database -> 'K -> 'T * Database

let inline hash x =
  System.Collections.Generic.EqualityComparer.Default.GetHashCode(x)

let dbEmpty () =
  {
    Serial = 0
    Cache = []
    Stack = []
  }

let dbDoFindQueryCache theKh theQh (db: Database) =
  let rec go cache =
    match cache with
    | (kh, qh, rh, result, deps) :: _ when kh = theKh && qh = theQh ->
      // eprintfn "found cache (%d, %d, %d, %A, %A)" kh qh rh result deps
      Some (rh, result, deps)

    | _ :: cache ->
      go cache

    | [] ->
      None

  go db.Cache

let dbFindQueryCache kh qh db =
  let rec go kh qh theRh db =
    match dbDoFindQueryCache kh qh db with
    | Some (rh, result, deps) when (theRh < 0 || rh = theRh) ->
      if deps |> List.forall (fun (kh, qh, rh) -> go kh qh rh db |> Option.isSome) then
        // eprintfn "reuse (%d, %d, %d, %A, %A)" kh qh rh result deps
        Some (rh, result)
      else
        // eprintfn "fresh (%d, %d, %d)" kh qh rh
        None

    | _ ->
      None

  go kh qh (-1) db

let dbRunQuery (key: 'K) qh (db: Database) (compute: Query<'K, 'T>): 'T * Database =
  let kh = hash key
  let oldStack = db.Stack

  match dbFindQueryCache kh qh db with
  | Some (rh, result) ->
    unbox result, { db with Stack = (kh, qh, rh) :: oldStack }

  | _ ->

  let db = { db with Stack = [] }

  let result, db = compute db key

  let rh = hash result
  let deps = db.Stack
  let db =
    { db with
        Cache = (kh, qh, rh, box result, deps) :: db.Cache
        Stack = (kh, qh, rh) :: oldStack
    }

  // eprintfn "kh=%d qh=%d result=%A deps=%A" kh qh result deps
  result, db

let dbNewQuery db compute =
  let qh = db.Serial + 1
  let db = { db with Serial = db.Serial + 1 }

  let rec q db key = dbRunQuery key qh db compute
  q, qh, db

let queryInput db value =
  let cell = ref value
  let q, qh, db = dbNewQuery db (fun db () -> ! cell, db)
  let set newValue db =
    eprintfn "set %d" qh
    cell := newValue
    { db with Cache = db.Cache |> List.filter (fun (_, theQh, _, _, _) -> theQh <> qh) }
  q, set, db

let queryCompute db compute =
  let q, _, db = dbNewQuery db compute
  q, db

let queryRec db compute =
  let queryCell = ref Unchecked.defaultof<_>
  let q, _, db = dbNewQuery db (compute queryCell)
  queryCell := q
  q, db

let basicExample () =
  let db = dbEmpty ()

  let x, setX, db =
    queryInput db 0

  let y, setY, db =
    queryInput db 0

  let xx, db =
    queryCompute db (fun db () ->
      eprintfn "computing xx..."
      let x, db = x db ()
      x * x, db
    )

  let yy, db =
    queryCompute db (fun db () ->
      eprintfn "computing yy..."
      let y, db = x db ()
      y * y, db
    )

  let tan, db =
    queryCompute db (fun db () ->
      eprintfn "computing tan..."
      let x, db = x db ()
      if x = 0 then
        infinity, db
      else
        let y, db = y db ()
        (float y / float x), db
    )

  let show name query db =
    let value, db = query db ()
    printfn "%s = %A" name value
    db

  let db =
    db
    |> show "x" x
    |> show "xx" xx
    |> show "tan" tan
    |> setX 3
    |> setY 6
    |> show "x" x
    |> show "xx" xx
    |> show "xx" xx
    |> show "tan" tan
    |> show "tan" tan

  ()

type Ast =
  | Int
    of int
  | Add
    of Ast * Ast

let evaluateExample () =
  let db = dbEmpty ()

  let evaluate, db =
    queryRec db (fun evaluateRec db (ast: Ast) ->
      eprintfn "TRACE: evaluate %A" ast
      match ast with
      | Int value ->
        value, db

      | Add (first, second) ->
        let first, db = (! evaluateRec) db first
        let second, db = (! evaluateRec) db second
        first + second, db
    )

  let show ast db =
    let value, db = evaluate db ast
    eprintfn "%A #=> %d" ast value
    db

  db
  |> show (Add (Int 1, Int 1))
  |> show (Add (Add (Int 1, Int 1), Int 2))
  |> show (Add (Add (Int 1, Int 1), (Add (Int 2, Int 3))))
  |> ignore

[<EntryPoint>]
let main _ =
  // basicExample ()
  evaluateExample ()
  0

(*

TRACE: evaluate Add (Int 1,Int 1)
TRACE: evaluate Int 1
Add (Int 1,Int 1) #=> 2
TRACE: evaluate Add (Add (Int 1,Int 1),Int 2)
TRACE: evaluate Int 2
Add (Add (Int 1,Int 1),Int 2) #=> 4
TRACE: evaluate Add (Add (Int 1,Int 1),Add (Int 2,Int 3))
TRACE: evaluate Add (Int 2,Int 3)
TRACE: evaluate Int 3
Add (Add (Int 1,Int 1),Add (Int 2,Int 3)) #=> 7

*)
