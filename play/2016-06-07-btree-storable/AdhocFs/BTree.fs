namespace AdhocFs

open System

module BTree =
  let m = 5
  let hm = m / 2

  type System.Collections.Generic.List<'x> with
    member this.PopAt(i) =
      let x = this.[i]
      this.RemoveAt(i)
      x

  [<AbstractClass>]
  type Node<'k, 'v>() =
    let es = ResizeArray<'k * 'v>(m - 1)
    let ns = ResizeArray<option<Node<'k, 'v>>>(m)

    member this.Init(kv, l, r) =
      es.Add(kv)
      ns.Add(l)
      ns.Add(r)

    member this.Edges = es
    member this.Nodes = ns

    member this.Trim() =
      if ns.Count = 1
      then ns.[0]
      else this |> Some

    abstract member Deactivate: unit -> Node<'k, 'v>
    abstract member IsActive: bool

  type InactiveNode<'k, 'v>() =
    inherit Node<'k, 'v>()

    override this.Deactivate() = this :> Node<'k, 'v>
    override this.IsActive = false

  type ActiveNode<'k, 'v>(kv, l, r) as this =
    inherit Node<'k, 'v>()
    
    do this.Init(kv, l, r)

    override this.Deactivate() =
      let n = InactiveNode()
      n.Init(this.Edges.[0], this.Nodes.[0], this.Nodes.[1])
      n :> Node<'k, 'v>

    override this.IsActive = true

  type BTree<'k, 'v when 'k: comparison>() =
    let mutable _root = None

    member private this.Split(n: Node<'k, 'v>) =
      let j = hm
      let i = j - 1
      let r = InactiveNode()
      r.Edges.AddRange(n.Edges.GetRange(j, m - j - 1))
      r.Nodes.AddRange(n.Nodes.GetRange(j, m - j))
      n.Edges.RemoveRange(j, m - j - 1)
      n.Nodes.RemoveRange(j, m - j)
      ActiveNode(n.Edges.PopAt(i), Some n, Some (r :> Node<'k, 'v>))

    member private this.Balance(n: Node<'k, 'v>, i) =
      let ni = n.Nodes.[i].Value
      if ni.IsActive then
        n.Edges.Insert(i, ni.Edges.[0])
        n.Nodes.[i] <- ni.Nodes.[1]
        n.Nodes.Insert(i, ni.Nodes.[0])
        if n.Nodes.Count < m then n else this.Split(n) :> Node<'k, 'v>
      else
        n

    member private this.Insert(n: option<Node<'k, 'v>>, (k, v)) =
      match n with
      | None -> ActiveNode((k, v), None, None) :> Node<'k, 'v>
      | Some n ->
        let rec loop i =
          if i = n.Edges.Count then
            n.Nodes.[i] <- this.Insert(n.Nodes.[i], (k, v)) |> Some
            this.Balance(n, i)
          else
            let cmp = compare k (n.Edges.[i] |> fst)
            if cmp < 0 then
              n.Nodes.[i] <- this.Insert(n.Nodes.[i], (k, v)) |> Some
              this.Balance(n, i)
            else if cmp = 0 then
              n.Edges.[i] <- (k, v)
              n
            else
              loop (i + 1)
        in loop 0

    member this.Add(k, v) =
      _root <- this.Insert(_root, (k, v)).Deactivate() |> Some

    member private this.IsRemoving(n: option<Node<'k, 'v>>) =
      match n with
      | None -> false
      | Some n -> n.Nodes.Count < hm

    /// 余裕のある右のノードから枝を1本分けてもらう。
    member private this.MoveLR(e, l: Node<'k, 'v>, r: Node<'k, 'v>) =
      l.Edges.Add(e)
      l.Nodes.Add(r.Nodes.PopAt(0))
      r.Edges.PopAt(0)

    /// 余裕のある左のノードから枝を1本分けてもらう。
    member private this.MoveRL(e, l: Node<'k, 'v>, r: Node<'k, 'v>) =
      let j = l.Edges.Count
      let i = j - 1
      r.Edges.Insert(0, e)
      r.Nodes.Insert(0, l.Nodes.PopAt(j))
      l.Edges.PopAt(i)

    /// 削除時のアクティブなノードと反応してツリーを変形する。
    /// 最も右の部分木がアクティブな場合の処理。
    /// アクティブでなければ何もしない。
    member private this.BalanceR(n: Node<'k, 'v>, j) =
      let nj = n.Nodes.[j]
      if this.IsRemoving(nj) then
        let nj = nj |> Option.get
        let i = j - 1
        let e = n.Edges.[i]
        let ni = n.Nodes.[i].Value
        if ni.Nodes.Count = hm then
          ni.Edges.Add(e)
          ni.Edges.AddRange(nj.Edges)
          ni.Nodes.AddRange(nj.Nodes)
          n.Edges.RemoveAt(i)
          n.Nodes.RemoveAt(j)
        else
          n.Edges.[i] <- this.MoveLR(e, ni, nj)

    /// 削除時のアクティブなノードと反応してツリーを変形する。
    /// アクティブでなければ何もしない。
    member private this.BalanceL(n: Node<'k, 'v>, i) =
      let ni = n.Nodes.[i]
      if this.IsRemoving(ni) then
        let ni = ni |> Option.get
        let j = i + 1
        let e = n.Edges.[i]
        let nj = n.Nodes.[j].Value
        // nj に余裕がない場合 (融合)
        if nj.Nodes.Count = hm then
          ni.Edges.Add(e)
          ni.Edges.AddRange(nj.Edges)
          ni.Nodes.AddRange(nj.Nodes)
          n.Edges.RemoveAt(i)
          n.Nodes.RemoveAt(j)
        else
          n.Edges.[i] <- this.MoveRL(e, ni, nj)
          
    /// 部分木 n の最大値キーの要素を削除する。
    /// 削除したキーの要素を返す。
    member private this.RemoveMax(n: Node<'k, 'v>) =
      let j = n.Edges.Count
      let i = j - 1
      match n.Nodes.[j] with
      | None ->
        n.Nodes.RemoveAt(j)
        let p = n.Edges.[i]
        n.Edges.RemoveAt(i)
        p
      | Some r ->
        let e = this.RemoveMax(r)
        this.BalanceR(n, j)
        e

    member private this.Remove(n: option<Node<'k, 'v>>, k: 'k) =
      match n with
      | None -> ()
      | Some n ->
        let len = n.Edges.Count
        let rec loop i =
          if i = len then
            this.Remove(n.Nodes.[len], k)
            this.BalanceR(n, len)
          else
            let cmp = compare k (n.Edges.[i] |> fst)
            if cmp < 0 then
              this.Remove(n.Nodes.[i], k)
              this.BalanceL(n, i)
            else if cmp = 0 then
              match n.Nodes.[i] with
              | None ->
                n.Edges.RemoveAt(i)
                n.Nodes.RemoveAt(i)
              | Some r ->
                n.Edges.[i] <- this.RemoveMax(r)
                this.BalanceL(n, i)
            else
              loop (i + 1)
        in loop 0

    member this.Remove(k) =
      _root |> Option.iter (fun n ->
        this.Remove(Some n, k)
        _root <- _root.Value.Trim())

    member this.TryFind(k) =
      let rec walk: option<Node<'k, 'v>> -> option<'v> =
        function
        | None -> None
        | Some n ->
          let rec loop i =
            if i < n.Edges.Count then
              let cmp = compare k (n.Edges.[i] |> fst)
              if cmp = 0 then n.Edges.[i] |> snd |> Some
              else if cmp < 0 then walk n.Nodes.[i]
              else loop (i + 1)
            else
              walk n.Nodes.[i]
          in loop 0
      in walk _root

    member this.IsEmpty =
      _root = None

    member this.ToSeq =
      let rec walk: option<Node<'k, 'v>> -> seq<'k * 'v> =
        function
        | None -> Seq.empty
        | Some n ->
          seq {
            let len = n.Edges.Count
            let rec loop i =
              seq {
                if i < len then
                  yield! walk n.Nodes.[i]
                  yield n.Edges.[i]
                  yield! loop (i + 1)
              }
            yield! loop 0
            yield! walk n.Nodes.[len]
          }
      in walk _root

  let private (|BT|) (x: BTree<'k, 'v>) = x

  let empty<'k, 'v when 'k: comparison> =
    BTree<'k, 'v>()

  let toSeq (BT this): seq<'k * 'v> =
    this.ToSeq

  let toList this = this |> toSeq |> Seq.toList

  let length this = this |> toSeq |> Seq.length

  let tryFind (k: 'k) (BT this): option<'v> =
    this.TryFind(k)

  let add k v (BT this) =
    this.Add(k, v)

  let remove k (BT this) =
    this.Remove(k)
