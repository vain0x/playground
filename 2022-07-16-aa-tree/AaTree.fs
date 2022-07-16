module rec AaTree

// -----------------------------------------------
// Stats
// -----------------------------------------------

let private skewCount = ref 0
let private splitRCount = ref 0
let private insertCount = ref 0
let private concatCount = ref 0
let private split2Count = ref 0

let resetStats () =
  skewCount.contents <- 0
  splitRCount.contents <- 0
  insertCount.contents <- 0
  concatCount.contents <- 0
  split2Count.contents <- 0

let printStats () =
  printfn
    "stats: skew:%d splitR:%d insert:%d concat:%d split:%d\ntotal: %d"
    (skewCount.contents)
    splitRCount.contents
    insertCount.contents
    concatCount.contents
    split2Count.contents
    (skewCount.contents
     + splitRCount.contents
     + insertCount.contents
     + concatCount.contents
     + split2Count.contents)

// -----------------------------------------------
// Nodes
// -----------------------------------------------

[<NoEquality; NoComparison>]
type private Node<'K, 'T> =
  | E
  | T of level: int * left: Node<'K, 'T> * key: 'K * value: 'T * right: Node<'K, 'T>

let private isEmpty node =
  match node with
  | E -> true
  | _ -> false

let private levelOf node =
  match node with
  | E -> 0
  | T (level, _, _, _, _) -> level

let private hasHorizontalRightLink node =
  match node with
  | T (level, _, _, _, T (rLev, _, _, _, _)) ->
    assert (rLev = level - 1 || rLev = level)
    level <= rLev
  | _ -> false

let private nodeSize node =
  match node with
  | E -> 0
  | T (_, l, _, _, r) -> nodeSize l + 1 + nodeSize r

// For debug
let private dumpNode (node: Node<'K, 'T>) =
  let display (k: 'K) (v: 'T) = string (box k) + ":" + string (box v)

  let rec go indent node =
    match node with
    | E -> printfn "%s(empty)" indent

    | T (level, l, k, v, r) ->
      let deep = indent + "  "
      go deep l

      printfn "%sN(%s)'%d" indent (display k v) (int level)

      if level > 1 && isEmpty l && isEmpty r then
        printfn "%s! Invalid leaf level" indent

      if levelOf l + 1 <> level then
        printfn "%s! Invalid left-child level" indent

      if levelOf r <> level && levelOf r + 1 <> level then
        printfn "%s! Invalid right-child level" indent

      match r with
      | T (_, _, _, _, T (rrLev, _, _, _, _)) ->
        if rrLev >= level then
          printfn "%s! Invalid right-right-child level" indent

      | _ -> ()

      go deep r

  printfn "TreeMap:"
  go "  " node

// For debug
let private checkNode (node: Node<'K, 'T>) =
  let rec go node =
    match node with
    | E -> true

    | T (level, l, _, _, r) ->
      let c1 = level = 1 || not (isEmpty l && isEmpty r)
      let c2 = levelOf l + 1 = level
      let c3 = levelOf r = level || levelOf r + 1 = level

      let c4 =
        match r with
        | T (_, _, _, _, T (rrLev, _, _, _, _)) -> rrLev < level
        | _ -> true

      c1 && c2 && c3 && c4

  go node

module private TreeNode =
  /// `skew` 操作。左制約を回復するため、必要なら右回転する
  // body and result: fields of T
  let skew body : int * Node<'K, 'T> * 'K * 'T * Node<'K, 'T> =
    incr skewCount

    match body with
    | lev, T (lLev, ll, lk, lv, lr), k, v, r when lev = lLev ->
      // 左のノードが水平リンクを持っていたら壊れる気がする
      assert (levelOf lr = lev - 1)
      let newR = T(lev, lr, k, v, r)

      if not (checkNode newR) then
        dumpNode (T body)
        failwith "skew produced invalid node"

      lLev, ll, lk, lv, newR

    | _ -> body

  /// `split` 操作。孫制約を回復するため、必要なら左回転してレベルを上げる
  let splitR body : int * Node<'K, 'T> * 'K * 'T * Node<'K, 'T> =
    incr splitRCount

    match body with
    | lev, l, k, v, T (rLev, rl, rk, rv, (T (rrLev, _, _, _, _) as rr)) when lev = rrLev ->
      if lev <> rLev then dumpNode (T body) // debug
      assert (lev = rLev)

      lev + 1, T(lev, l, k, v, rl), rk, rv, rr

    | _ -> body

  let findFromNode (keyCompare: 'K -> 'K -> int) key node : 'T option =
    let rec go node =
      match node with
      | E -> None

      | T (_, l, k, v, r) ->
        let c = keyCompare key k

        if c < 0 then
          // key < k
          go l
        else if c > 0 then
          // k < key
          go r
        else
          // key = k
          Some v

    go node

  let insertNode (keyCompare: 'K -> 'K -> int) (newKey: 'K) (newValue: 'T) node : Node<'K, 'T> =
    let rec go node : int * Node<'K, 'T> * 'K * 'T * Node<'K, 'T> =
      incr insertCount

      match node with
      | E -> 1, E, newKey, newValue, E

      | T (level, l, k, v, r) ->
        let c = keyCompare newKey k

        let node =
          if c < 0 then
            // key < k
            level, T(go l), k, v, r
          else if c > 0 then
            // k < key
            level, l, k, v, T(go r)
          else
            // key = k
            level, l, newKey, newValue, r

        splitR (skew node)

    T(go node)


  let removeNode (keyCompare: 'K -> 'K -> int) key node : 'T option * Node<'K, 'T> =
    let rec successor l k v =
      match l with
      | E -> k, v
      | T (_, l, k, v, _) -> successor l k v

    let rec predecessor k v r =
      match r with
      | E -> k, v
      | T (_, _, k, v, r) -> predecessor k v r

    let decreaseLevel body : int * Node<'K, 'T> * 'K * 'T * Node<'K, 'T> =
      let lev, l, k, v, r = body
      let low = min (levelOf l) (levelOf r) + 1

      if lev > low then
        let r =
          match r with
          | T (rLev, rl, rk, rv, rr) when rLev > low -> T(low, rl, rk, rv, rr)
          | _ -> r

        low, l, k, v, r
      else
        body

    let rebalance (body: int * Node<'K, 'T> * 'K * 'T * Node<'K, 'T>) : Node<'K, 'T> =
      let body = body |> decreaseLevel |> skew

      let level, l, k, v, r = body

      let r =
        match r with
        | E -> E

        | T (rLevel, rl, rk, rv, rr) ->
          let rr =
            match rr with
            | E -> E
            | T (rrLevel, rrl, rrk, rrv, rrr) -> skew (rrLevel, rrl, rrk, rrv, rrr) |> T

          (rLevel, rl, rk, rv, rr) |> skew |> T

      let level, l, k, v, r = splitR (level, l, k, v, r)

      let r =
        match r with
        | E -> E
        | T (rLevel, rl, rk, rv, rr) -> splitR (rLevel, rl, rk, rv, rr) |> T

      T(level, l, k, v, r)

    let rec go node =
      match node with
      | E -> None, E

      | T (level, l, k, v, r) ->
        let c = keyCompare key k

        if c < 0 then
          // key < k
          let removed, l = go l
          removed, rebalance (level, l, k, v, r)
        else if c > 0 then
          // k < key
          let removed, r = go r
          removed, rebalance (level, l, k, v, r)
        else
          let removed = Some v

          match l, r with
          | T (_, _, lk, lv, lr), _ ->
            let pk, pv = predecessor lk lv lr
            let _, l = removeNode keyCompare pk l
            removed, rebalance (level, l, pk, pv, r)

          | _, T (_, rl, rk, rv, _) ->
            let pk, pv = successor rl rk rv
            let _, r = removeNode keyCompare pk r
            removed, rebalance (level, l, pk, pv, r)

          | _ -> removed, E

    go node

// -----------------------------------------------
// TreeMap
// -----------------------------------------------

type TreeMap<'K, 'T> = private TreeMap of Node<'K, 'T> * keyCompare: ('K -> 'K -> int)

module TMap =
  let empty (keyCompare: 'K -> 'K -> int) : TreeMap<'K, 'T> = TreeMap(E, keyCompare)

  let isEmpty (map: TreeMap<_, _>) : bool =
    let (TreeMap (node, _)) = map

    match node with
    | E -> true
    | _ -> false

  let tryFind (key: 'K) (map: TreeMap<'K, 'T>) : 'T option =
    let (TreeMap (node, keyCompare)) = map
    TreeNode.findFromNode keyCompare key node

  let containsKey (key: 'K) (map: TreeMap<'K, _>) : bool =
    let (TreeMap (node, keyCompare)) = map

    match TreeNode.findFromNode keyCompare key node with
    | Some _ -> true
    | None -> false

  let add (key: 'K) (value: 'T) (map: TreeMap<'K, 'T>) : TreeMap<'K, 'T> =
    let (TreeMap (node1, keyCompare)) = map

    let node2 = TreeNode.insertNode keyCompare key value node1

    if not (checkNode node2) then
      eprintfn "node1:"
      dumpNode node1
      eprintfn "node1:"
      dumpNode node2
      failwithf "add(%A:%A) produced invalid node" key value

    TreeMap(node2, keyCompare)

  let remove (key: 'K) (map: TreeMap<'K, 'T>) : 'T option * TreeMap<'K, 'T> =
    let (TreeMap (node1, keyCompare)) = map
    let valueOpt, node2 = TreeNode.removeNode keyCompare key node1

    if not (checkNode node2) then
      eprintfn "node1:"
      dumpNode node1
      eprintfn "node1:"
      dumpNode node2
      failwithf "remove(%A) produced invalid node" key

    valueOpt, TreeMap(node2, keyCompare)

  let ofList keyCompare (assoc: ('K * 'T) list) : TreeMap<'K, 'T> =
    let node =
      assoc
      |> List.fold (fun node (k, v) -> TreeNode.insertNode keyCompare k v node) E

    if not (checkNode node) then
      eprintfn "input: %A" assoc
      eprintfn "node:"
      dumpNode node
      failwith "ofList produced invalid node"

    TreeMap(node, keyCompare)

  let toList (map: TreeMap<'K, 'T>) : ('K * 'T) list =
    let rec go acc node =
      match node with
      | E -> acc
      | T (_, l, k, v, r) -> go ((k, v) :: (go acc l)) r

    let (TreeMap (node, _)) = map
    go [] node

  let check (map: TreeMap<'K, 'T>) =
    let (TreeMap (node, _)) = map
    checkNode node

  let dump (map: TreeMap<'K, 'T>) =
    let (TreeMap (node, _)) = map
    dumpNode node
