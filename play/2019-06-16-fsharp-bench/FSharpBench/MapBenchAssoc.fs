module rec FSharpBench.MapBenchAssoc

type AssocMap<'K, 'V> = (int * ('K * 'V) list) list * ('K -> int) * ('K -> 'K -> int)

let listIsEmpty xs =
  match xs with
  | [] ->
    true

  | _ ->
    false

let listLength xs =
  let rec go len xs =
    match xs with
    | [] ->
      len

    | _ :: xs ->
      go (len + 1) xs

  go 0 xs

let listRev xs =
  let rec go acc xs =
    match xs with
    | [] ->
      acc

    | x :: xs ->
      go (x :: acc) xs

  go [] xs

let rec listIter f xs =
  match xs with
  | [] ->
    ()

  | x :: xs ->
    f x
    listIter f xs

let listMap f xs =
  let rec go acc xs =
    match xs with
    | [] ->
      listRev acc

    | x :: xs ->
      go (f x :: acc) xs

  go [] xs

let listMapWithIndex f xs =
  let rec go acc i xs =
    match xs with
    | [] ->
      listRev acc

    | x :: xs ->
      go (f i x :: acc) (i + 1) xs

  go [] 0 xs

let listFilter pred xs =
  let rec go acc xs =
    match xs with
    | [] ->
      listRev acc

    | x :: xs ->
      let acc = if pred x then x :: acc else acc
      go acc xs

  go [] xs

let listChoose f xs =
  let rec go acc xs =
    match xs with
    | [] ->
      listRev acc

    | x :: xs ->
      let acc =
        match f x with
        | Some y ->
          y :: acc

        | None ->
          acc

      go acc xs

  go [] xs

let listCollect f xs =
  let rec gogo acc ys =
    match ys with
    | [] ->
      acc

    | y :: ys ->
      gogo (y :: acc) ys

  let rec go acc xs =
    match xs with
    | [] ->
      listRev acc

    | x :: xs ->
      let acc = gogo acc (f x)
      go acc xs

  go [] xs

let listForAll pred xs =
  let rec go xs =
    match xs with
    | [] ->
      true

    | x :: xs ->
      pred x && go xs

  go xs

let listExists pred xs =
  let rec go xs =
    match xs with
    | [] ->
      false

    | x :: xs ->
      pred x || go xs

  go xs

/// USAGE: `items |> listFold (fun state item -> nextState) initialState`
let listFold folder state xs =
  let rec go state xs =
    match xs with
    | [] ->
      state

    | x :: xs ->
      go (folder state x) xs

  go state xs

let listReduce reducer xs =
  match xs with
  | [] ->
    failwith "listReduce: empty"

  | x :: xs ->
    listFold reducer x xs

let listLast xs =
  let rec go xs =
    match xs with
    | [] ->
      failwith "listLast: empty"

    | [x] ->
      x

    | _ :: xs ->
      go xs

  go xs

let listSkip count xs =
  let rec go count xs =
    match xs with
    | [] ->
      []

    | _ when count <= 0 ->
      xs

    | _ :: xs ->
      go (count - 1) xs

  go count xs

let listTruncate count xs =
  let rec go acc count xs =
    match xs with
    | [] ->
      listRev acc

    | _ when count <= 0 ->
      listRev acc

    | x :: xs ->
      go (x :: acc) (count - 1) xs

  go [] count xs

let listAppend xs ys =
  let rec go acc xs =
    match xs with
    | [] ->
      acc

    | x :: xs ->
      go (x :: acc) xs

  go ys (listRev xs)

let listSortCore unique cmp xs =
  // `merge (xs, xn) (ys, yn) = (zs, zn), d` where
  // `zs.[0..zn - 1]` is the merge of `xs.[0..xn - 1]` and `ys.[0..yn - 1]`,
  // and `d` is the number of duplicated items.
  let rec merge (xs, xn) (ys, yn) =
    if xn = 0 then
      (ys, yn), 0
    else if yn = 0 then
      (xs, xn), 0
    else
      match xs, ys with
      | [], _
      | _, [] ->
        failwith "NEVER: wrong list length"

      | x :: xs1, y :: ys1 ->
        let c = cmp x y
        if c > 0 then
          let (zs, zn), d = merge (xs, xn) (ys1, yn - 1)
          assert (zn + d = xn + (yn - 1))
          (y :: zs, zn + 1), d
        else if c = 0 && unique then
          let (zs, zn), d = merge (xs, xn) (ys1, yn - 1)
          assert (zn + d = xn + (yn - 1))
          (zs, zn), d + 1
        else
          let (zs, zn), d = merge (xs1, xn - 1) (ys, yn)
          assert (zn + d = (xn - 1) + yn)
          (x :: zs, zn + 1), d

  // `go (xs, xn) = (zs, zn), xs1, d` where
  // `zs.[0..xn - 1]` is the sort of `xs.[0..xn - 1]`,
  // `xs1 = xs.[xn..]`,
  // and `d` is the number of duplicated items.
  let rec go (xs, n) =
    if n <= 1 then
      (xs, n), listSkip n xs, 0
    else
      let m = n / 2
      let (xs, xn), xs1, d1 = go (xs, m)
      let (ys, yn), ys1, d2 = go (xs1, n - m)
      let (zs, zn), d3 = merge (xs, xn) (ys, yn)
      (zs, zn), ys1, d1 + d2 + d3

  let xn = listLength xs
  let (zs, zn), ws, d = go (xs, xn)
  assert (zn + d = xn)
  assert (ws |> listIsEmpty)
  listTruncate zn zs

let listSort cmp xs =
  listSortCore false cmp xs

let listUnique cmp xs =
  listSortCore true cmp xs

// -----------------------------------------------
// Assoc
// -----------------------------------------------

let assocAdd key value assoc =
  (key, value) :: assoc

let assocRemove cmp key assoc =
  let rec go acc assoc =
    match assoc with
    | [] ->
      listRev acc

    | (k, _) :: assoc
      when cmp k key = 0 ->
      go acc assoc

    | kv :: assoc ->
      go (kv :: acc) assoc

  go [] assoc

let assocTryFind cmp key assoc =
  let rec go assoc =
    match assoc with
    | [] ->
      None

    | (k, v) :: _
      when cmp k key = 0 ->
      Some v

    | _ :: assoc ->
      go assoc

  go assoc

let assocFold folder state assoc =
  let rec go state assoc =
    match assoc with
    | [] ->
      state

    | (k, v) :: assoc ->
      go (folder state k v) assoc

  go state assoc

let assocMap f assoc =
  let rec go acc assoc =
    match assoc with
    | [] ->
      listRev acc

    | (k, v) :: assoc ->
      go ((k, f k v) :: acc) assoc

  go [] assoc

let assocToKeyAcc acc assoc =
  let rec go acc assoc =
    match assoc with
    | [] ->
      acc

    | (k, _) :: assoc ->
      go (k :: acc) assoc

  go acc assoc

// -----------------------------------------------
// HashTrie
// -----------------------------------------------

let trieAdd (keyHash: int) key value trie =
  let rec go trie =
    match trie with
    | [] ->
      [keyHash, [key, value]]

    | (h, assoc) :: trie
      when h = keyHash ->
      (keyHash, assocAdd key value assoc) :: trie

    | kv :: trie ->
      kv :: go trie

  go trie

let trieRemove cmp (keyHash: int) key trie =
  let rec go trie =
    match trie with
    | [] ->
      []

    | (h, assoc) :: trie
      when h = keyHash ->
      (keyHash, assocRemove cmp key assoc) :: trie

    | kv :: trie ->
      kv :: go trie

  go trie

let trieTryFind cmp (keyHash: int) key trie =
  let rec go trie =
    match trie with
    | [] ->
      None

    | (h, assoc) :: _
      when h = keyHash ->
      assocTryFind cmp key assoc

    | _ :: trie ->
      go trie

  go trie

let trieMap f trie =
  let rec go trie =
    match trie with
    | [] ->
      []

    | (h, assoc) :: trie ->
      (h, assocMap f assoc) :: go trie

  go trie

let trieToKeys trie =
  let rec go acc trie =
    match trie with
    | [] ->
      acc

    | (_, assoc) :: trie ->
      go (assocToKeyAcc acc assoc) trie

  go [] trie

// -----------------------------------------------
// AssocMap
// -----------------------------------------------

let mapEmpty (hash, cmp): AssocMap<_, _> =
  [], hash, cmp

let mapAdd key value (trie, hash, cmp): AssocMap<_, _> =
  let trie = trie |> trieAdd (hash key) key value
  trie, hash, cmp

let mapRemove key (trie, hash, cmp): AssocMap<_, _> =
  let trie = trie |> trieRemove cmp (hash key) key
  trie, hash, cmp

let mapTryFind key ((trie, hash, cmp): AssocMap<_, _>) =
  trie |> trieTryFind cmp (hash key) key

let mapFind key map =
  match mapTryFind key map with
  | Some value ->
    value

  | None ->
    failwithf "mapFind: missing key (%A)" key

let mapContainsKey key map =
  match mapTryFind key map with
  | Some _ ->
    true

  | None ->
    false

let mapFold folder state (map: AssocMap<_, _>) =
  map |> mapToList |> assocFold folder state

let mapMap f (trie, hash, cmp): AssocMap<_, _> =
  let trie = trieMap f trie
  trie, hash, cmp

let mapToList (map: AssocMap<_, _>) =
  let trie, _, cmp = map

  let rec go acc keys =
    match keys with
    | [] ->
      acc

    | key :: keys ->
      go ((key, mapFind key map) :: acc) keys

  // Sort in reversed order and re-reverse it with `go`.
  trie |> trieToKeys |> listUnique (fun l r -> cmp r l) |> go []

let mapOfList (hash, cmp) assoc: AssocMap<_, _> =
  /// Partition an assoc by hash of key to acc/rest.
  let rec group keyHash acc others assoc =
    match assoc with
    | [] ->
      acc, others

    | ((key, _) as kv) :: assoc ->
      if hash key = keyHash then
        group keyHash (kv :: acc) others assoc
      else
        group keyHash acc (kv :: others) assoc

  let rec go trie assoc =
    match assoc with
    | [] ->
      trie

    | ((key, _) as kv) :: assoc ->
      let h = hash key
      let acc, assoc = group h [kv] [] assoc
      go ((h, acc) :: trie) assoc

  let trie = go [] assoc
  trie, hash, cmp

let intCmp (x: int) (y: int) =
  if y < x then
    1
  else if y = x then
    0
  else
    -1

let intHash (x: int) =
  x &&& 511
