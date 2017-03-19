namespace VainZero.Reading.Pfds

open System.Runtime.CompilerServices
open VainZero.Reading.Pfds.Chapter0301

module Chapter0302 =
  (*
  binomial heap

  2進表現ヒープと訳したい。

  binomial heap は binomial tree という多分ツリーを内部に含む。

  ランク 0 の binomial tree は1点。
  ランク i の binomial tree は、
    ランク (i - 1) の binomial tree の根から、
    同ランクの binomial tree の根に辺を張ったもの。

  Property: ランク r の binomial tree のサイズは 2^r に等しい。

  binomial heap は、サイズ n を2進展開した結果が (b_i) だとすると、
  b_r = 1 となる各 r に対応するランク r の binomial tree を1個ずつ含む。
  *)

  type private Rank = int

  type BinomialTree<'x when 'x: comparison> =
    /// ノードは、ランク、要素、子ノードのリストからなる。
    /// 子ノードのリストは、ランクについて降順に並んでいる。
    /// 要素は heap-order で並ぶ。
    | Node of Rank * 'x * list<BinomialTree<'x>>

  type BinomialHeap<'x when 'x: comparison> =
    | Heap of list<BinomialTree<'x>>

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module BinomialTree =
    let rank (Node (r, _, _)) = r
    let element (Node (_, x, _)) = x
    let children (Node (_, _, ch)) = ch

    let singleton x =
      Node (0, x, [])

    /// Links a binomial tree with another one.
    /// Expects they have the same rank.
    let link l r =
      assert (rank l = rank r)
      /// heap-order で並べるため、要素の小さい方を親とする。
      if element l < element r then
        Node (rank l + 1, element l, r :: children l)
      else
        Node (rank l + 1, element r, l :: children r)

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module BinomialHeap =
    let unwrap (Heap ts) = ts

    let empty = Heap []

    let isEmpty h =
      h = empty

    /// Takes O(log n) time because the heap contains at most O(log n) trees.
    /// Remember the binay representation.
    let rec internal insertTree t (Heap ts) =
      match ts with
      | [] ->
        [t] |> Heap
      | t' :: ts' ->
        assert (BinomialTree.rank t <= BinomialTree.rank t')
        if BinomialTree.rank t < BinomialTree.rank t' then
          t :: ts |> Heap
        else
          Heap ts' |> insertTree (BinomialTree.link t t')

    /// O(log n) time.
    let rec insert x h =
      h |> insertTree (BinomialTree.singleton x)

    /// O(log n) time.
    let rec merge l r =
      match (l, r) with
      | (l, Heap []) -> l
      | (Heap [], r) -> r
      | (Heap (lt :: lts), Heap (rt :: rts)) ->
        let comparison = compare (BinomialTree.rank lt) (BinomialTree.rank rt)
        if comparison < 0 then
          lt :: (merge (Heap lts) r |> unwrap) |> Heap
        else if comparison > 0 then
          rt :: (merge l (Heap rts) |> unwrap) |> Heap
        else
          insertTree (BinomialTree.link lt rt) (merge (Heap lts) (Heap rts))

    let removeMinTree =
      /// Returns the tree with the minimum element among (t :: ts) and the rest trees.
      /// Takes O(log n) time.
      let rec loop t ts =
        match ts with
        | [] ->
          (t, [])
        | t' :: ts' ->
          let (t', ts') = loop t' ts'
          if BinomialTree.element t < BinomialTree.element t' then
            (t, ts)
          else
            (t', t :: ts')
      function
      | Heap [] ->
        None
      | Heap (t :: ts) ->
        Some (loop t ts)

    /// O(log n) time.
    let findMin h =
      h |> removeMinTree |> Option.map (fst >> BinomialTree.element)

    /// O(log n) time.
    let deleteMin h =
      h |> removeMinTree
      |> Option.map
        (fun (t, ts) ->
          let subheap =
            merge
              (t |> BinomialTree.children |> List.rev |> Heap)
              (Heap ts)
          (BinomialTree.element t, subheap)
        )

    let ofSeq xs =
      xs |> Seq.fold (fun h x -> h |> insert x) empty

    let rec toSeq h =
      seq {
        match h |> deleteMin with
        | None -> ()
        | Some (x, h') ->
          yield x
          yield! h' |> toSeq
      }

    let signature =
      {
        Empty           = empty
        IsEmpty         = isEmpty
        Insert          = insert
        Merge           = merge
        FindMin         = findMin
        DeleteMin       = deleteMin
        OfSeq           = ofSeq
      }

  // Ex3.5
  module Exercise05 =
    module BinomialHeap =
      let findMin' =
        function
        | Heap [] ->
          None
        | Heap ts ->
          ts |> List.map BinomialTree.element |> Seq.min |> Some

      let signature' () =
        { BinomialHeap.signature with FindMin = findMin' }

  module Exercise06 =
    type BinomialTree<'x when 'x: comparison> =
      | Node of 'x * list<BinomialTree<'x>>

    type BinomialHeap<'x when 'x: comparison> =
      | Heap of list<Rank * BinomialTree<'x>>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module BinomialTree =
      let element (Node (x, _)) = x
      let children (Node (_, ch)) = ch

      let singleton x =
        Node (x, [])

      let link l r =
        if element l < element r then
          Node (element l, r :: children l)
        else
          Node (element r, l :: children r)

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module BinomialHeap =
      let unwrap (Heap ts) = ts

      let empty = Heap []

      let isEmpty h =
        h = empty

      let rank (r, (_: BinomialTree<_>)) = r
      let tree (_, t) = t: BinomialTree<_>

      let link l r =
        assert (rank l = rank r)
        (rank l + 1, BinomialTree.link (tree l) (tree r))

      /// Takes O(log n) time because the heap contains at most O(log n) trees.
      /// Remember the binay representation.
      let rec internal insertTree t (Heap ts) =
        match ts with
        | [] ->
          [t] |> Heap
        | t' :: ts' ->
          assert (rank t <= rank t')
          if rank t < rank t' then
            t :: ts |> Heap
          else
            Heap ts' |> insertTree (link t t')

      /// O(log n) time.
      let rec insert x h =
        h |> insertTree (0, BinomialTree.singleton x)

      /// O(log n) time.
      let rec merge l r =
        match (l, r) with
        | (l, Heap []) -> l
        | (Heap [], r) -> r
        | (Heap (lt :: lts), Heap (rt :: rts)) ->
          let comparison = compare (rank lt) (rank rt)
          if comparison < 0 then
            lt :: (merge (Heap lts) r |> unwrap) |> Heap
          else if comparison > 0 then
            rt :: (merge l (Heap rts) |> unwrap) |> Heap
          else
            insertTree (link lt rt) (merge (Heap lts) (Heap rts))

      let removeMinTree =
        /// Returns the tree with the minimum element among (t :: ts) and the rest trees.
        /// Takes O(log n) time.
        let rec loop t ts =
          match ts with
          | [] ->
            (t, [])
          | t' :: ts' ->
            let (t', ts') = loop t' ts'
            let e = tree >> BinomialTree.element
            if e t < e t' then
              (t, ts)
            else
              (t', t :: ts')
        function
        | Heap [] ->
          None
        | Heap (t :: ts) ->
          Some (loop t ts)

      /// O(log n) time.
      let findMin h =
        h |> removeMinTree |> Option.map (fst >> tree >> BinomialTree.element)

      /// O(log n) time.
      let deleteMin h =
        h |> removeMinTree
        |> Option.map
          (fun (t, ts) ->
            let subheap =
              merge
                (t |> tree |> BinomialTree.children |> List.rev
                  |> List.mapi (fun i t -> (i, t)) |> Heap)
                (Heap ts)
            (t |> tree |> BinomialTree.element, subheap)
          )

      let ofSeq xs =
        xs |> Seq.fold (fun h x -> h |> insert x) empty

      let rec toSeq h =
        seq {
          match h |> deleteMin with
          | None -> ()
          | Some (x, h') ->
            yield x
            yield! h' |> toSeq
        }

      let signature =
        {
          Empty           = empty
          IsEmpty         = isEmpty
          Insert          = insert
          Merge           = merge
          FindMin         = findMin
          DeleteMin       = deleteMin
          OfSeq           = ofSeq
        }

  module Exercise07 =
    type ExplicitMinHeap<'x, 'h> =
      option<'x * 'h>

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ExplicitMinHeap =
      let signature (heapSig: HeapSignature<'x, 'h>): HeapSignature<'x, ExplicitMinHeap<'x, 'h>> =
        let ofHeap h =
          h |> heapSig.FindMin |> Option.map (fun m -> (m, h))
        {
          Empty           = Option.None
          IsEmpty         = Option.isNone
          Insert          =
            fun x ->
              function
              | None ->
                (x, heapSig.Empty |> heapSig.Insert x) |> Some
              | Some (m, h) ->
                let h'    = h |> heapSig.Insert x
                (min m x, h') |> Some
          Merge           =
            fun l r ->
              match (l, r) with
              | (Some _, None) -> l
              | (None, Some _) -> r
              | (None, None) -> None
              | (Some (lm, lh), Some (rm, rh)) ->
                (min lm rm, heapSig.Merge lh rh) |> Some
          FindMin         =
            Option.map fst
          DeleteMin       =
            fun this ->
              this |> Option.bind (fun (m, h) ->
                heapSig.DeleteMin h |> Option.bind (fun (x, h') ->
                  Some (x, ofHeap h')
                ))
          OfSeq           =
            fun xs ->
              xs |> heapSig.OfSeq |> ofHeap
        }
