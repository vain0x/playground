namespace DotNetLab.Fs.Lib.PFDS

open System.Collections.Generic

module Chapter03 =
  type HeapSignature<'x, 'h when 'x: comparison> =
    {
      Empty             : 'h
      IsEmpty           : 'h -> bool
      Insert            : 'x -> 'h -> 'h
      Merge             : 'h -> 'h -> 'h
      FindMin           : 'h -> option<'x>
      DeleteMin         : 'h -> option<'x * 'h>
      OfSeq             : seq<'x> -> 'h
    }

  (*
    Heap-ordered tree:
      A tree in which the element at each node is no larger than the elements
      at its children.
      Implies that the root is minimum.

    Leftist property:
      For any node n in a leftist heap,
      the rank of the left child is at least as large as the right child.

    Right spine: The rightmost path from the node to an empty node.

    Rank of a node: Length of the right spine.

    Theorem.
      Right spine of any node is the shortest path to an empty node.
    Proof.
      構造的帰納法により示す。
      ノードが空の場合は自明。
      任意のノード n をとり、それが空ではないとする。
      n の任意の部分ノード n' について、
      n' の右背骨が n' から空ノードへの最短経路であると仮定する。
      r = rank(n) とおく。
      n から空のノードへの最短経路の長さを l とおく。
      l < r と仮定する。矛盾を導けば結論が得られる。
      n から空のノードへの最短経路 p を1つ選ぶ。
      p の最初の枝が右の場合:
        n の右の子ノード n' から空のノードへの最短経路の長さは l - 1 以下。
        帰納法の仮定より、n' から空のノードへの最短経路は右背骨に等しい。
        その長さは r - 1 であり、r - 1 = l - 1 < r - 1 となり矛盾を得る。
      p の最初の枝が左の場合:
        n の左の子ノードを n' とおく。
        帰納法の仮定より、n' から空のノードへの最短経路は右背骨である。
        その長さを r' とおくと、l = r' + 1。
        Leftist property より、r' >= r であるが、
        r' = l - 1 < r - 1 < r となり矛盾。
    End Proof.

    Exercise 3.1.
      The right spine of a leftist heap of size n
      contains at most [log(n + 1)] elements.
      (つまり、r を 1 増やすには、要素数をほぼ倍にする必要がある。
      実際 r を増やしてみると、leftist property に従うために、
      大量のノードを追加する必要があることが分かる。)
    Proof.
      構造的帰納法で示す。
      空でない任意のノード n をとる。
      r = rank(n) とおく。
      左の子ノードを l、右の子ノードを m とおく。
      l のランクを r' とおく。leftist property より r' >= r - 1。
      仮定より、
        size(l) >= 2^r' - 1 >= 2^(r - 1) - 1、
        size(m) >= 2^(r - 1) - 1。
      size(n)
        = size(l) + size(m) + 1
        = 2^(r - 1) - 1 + 2^(r - 1) - 1 + 1
        = 2^r - 1。
      すなわち
        2^r = size(n) + 1
      対数をとって
        r = log (2^r) <= log (size(n) + 1)
      r: 整数なので、
        r <= floor(log (size(n) + 1))
    End Proof.

  Key insight:
    Two heaps can be merged by merging their right spines
    as you would merge two sorted lists,
    and then swapping the children of nodes along this path as necessary
    to restore the leftist property.
  *)

  /// Represents a heap-ordered binary tree satisfying the leftist property.
  type LeftistHeap<'x when 'x: comparison> =
    private
    | Leaf
    | Node of 'x * rank: int * LeftistHeap<'x> * LeftistHeap<'x>
  with
    member this.Empty =
      Leaf

    member this.IsEmpty =
      match this with
      | Leaf -> true
      | Node _ -> false

    member this.RightSpine =
      let rec loop this =
        seq {
          match this with
          | Leaf -> ()
          | Node (_, x, _, right) ->
            yield x
            yield! loop right
        }
      in
        loop this

    member this.Rank =
      match this with
      | Leaf -> 0
      | Node (_, rank, _, _) -> rank

    /// Calculates the rank of a node
    /// and swaps its children if necessary.
    static member MakeNode(x, l, r) =
      let rank (heap: LeftistHeap<_>) =
        heap.Rank
      if rank l >= rank r then
        Node (x, rank r + 1, l, r)
      else
        Node (x, rank l + 1, r, l)

    /// Takes O(log n) time.
    member this.Merge(r) =
      let rec merge l r =
        match (l, r) with
        | (h, Leaf) -> h
        | (Leaf, h) -> h
        | (Node (lx, _, ll, lr), Node (rx, _, rl, rr)) ->
          if lx < rx then
            LeftistHeap<'x>.MakeNode(lx, ll, merge lr r)
          else
            LeftistHeap<'x>.MakeNode(rx, rl, merge l rr)
      in
        merge this r

    // Ex3.2
    member this.Insert(x) =
      //this.Merge(Node (x, 1, Leaf, Leaf))
      match this with
      | Leaf ->
        Node (x, 1, Leaf, Leaf)
      | Node (lx, _, ll, lr) ->
        if lx < x then
          LeftistHeap<'x>.MakeNode(lx, ll, lr.Merge(Node (x, 1, Leaf, Leaf)))
        else
          Node (x, 1, this, Leaf)

    member this.FindMin() =
      match this with
      | Leaf ->
        None
      | Node (x, _, _, _) ->
        Some x

    member this.DeleteMin() =
      match this with
      | Leaf ->
        None
      | Node (x, _, l, r) ->
        (x, l.Merge(r)) |> Some

    (*
      Ex3.3
      Property.
        This takes O(n) time.
      Proof.
        要素数が n である2つの leftist heap を merge するには、
        ある定数 c があって、t(n) = c・(log n + 1) かかる。
        このメソッドの漸近的時間計算量は、xs の長さを n とおくと、
          T(n)
            =Σ_i=0^∞ (n / 2^i)・t(i + 1)
            = c・(Σ_i=0^∞ 2^(-i)・log (i + 1))・n + C
        ここで、
          Σ_i=0^∞ 2^(-i)・log (i + 1) ～ 1.02
        より、
          T(n) = O(n)
      End Proof.
    *)
    static member OfSeq(xs) =
      let rec mergeAll (hs: seq<LeftistHeap<_>>) =
        hs
        |> Seq.fold
          (fun (acc, prev: option<LeftistHeap<_>>) h ->
            match prev with
            | Some l ->
              (l.Merge(h) :: acc, None)
            | None ->
              (acc, Some h)
          ) ([], None)
        |>
          function
          | ([], None) -> Leaf
          | ([], Some heap) -> heap
          | (acc, None) ->
            mergeAll acc
          | (acc, Some heap) ->
            mergeAll (heap :: acc)
      in
        xs
        |> Seq.map (fun x -> Node (x, 1, Leaf, Leaf))
        |> mergeAll

    static member AsHeap: HeapSignature<'x, LeftistHeap<'x>> =
      {
        Empty           = Leaf
        IsEmpty         = fun h -> h.IsEmpty
        Insert          = fun x h -> h.Insert(x)
        Merge           = fun l r -> l.Merge(r)
        FindMin         = fun h -> h.FindMin()
        DeleteMin       = fun h -> h.DeleteMin()
        OfSeq           = fun xs -> LeftistHeap<_>.OfSeq(xs)
      }

  (*
  Ex 3.4

  Weight-biased leftist heap:
    leftist heap の leftist property を
    weight-biased leftist property で置き換えて得られるデータ構造。

  Weight-biased leftist property:
    For any node n in the weight-biased leftist heap,
    the size of its left child is at least as large as that of the right one.

  (a)
  Property.
    The right spine of a weight-biased leftist heap
    contains at most [log(n + 1)] elements.
  Proof.
    TODO
  End Proof.
  *)
