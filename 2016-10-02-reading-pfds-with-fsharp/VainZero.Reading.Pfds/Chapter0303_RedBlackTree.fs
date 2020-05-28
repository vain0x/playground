namespace VainZero.Reading.Pfds

module Chapter0303 =
  open Chapter02.Exercise06

  type Color =
    | Red
    | Black

  (*
    RedBlackTree: 次の平衡不変条件 (balanace invariants) を満たす2分探索ツリー。
      1. 赤いノードは赤い子ノードを持たない。
      2. 根ノードから空ノードへのすべての経路について、その経路に含まれる黒いノードの個数が等しい。

    Property:
      The longest possible path in a red-black tree, one with alternating black and red nodes,
      is no more than twice as long as the shortest possible path, one with black nodes only.

    Property:
      The maximum depth of a node in a red-black tree of size n is at most 2 floor(log (n + 1)).
  *)

  type RedBlackTree<'k, 'v when 'k: comparison> =
    | Empty
    | Node of Color * ('k * 'v) * RedBlackTree<'k, 'v> * RedBlackTree<'k, 'v>

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module RedBlackTree =
    let empty = Empty

    let color =
      function
      | Empty ->
        Black
      | Node (c, _, _, _) ->
        c

    let rec tryFind k =
      function
      | Empty -> None
      | Node (_, (k', v'), left, right) ->
        let comparison = compare k k'
        if comparison < 0 then
          left |> tryFind k
        else if comparison > 0 then
          right |> tryFind k
        else
          Some v'

    let balance =
      let (|R|B|) =
        function
        | Red -> R
        | Black -> B
      let (|E|N|) =
        function
        | Empty ->
          E
        | Node (c, kv, l, r) ->
          N (c, l, kv, r)
      let r = Red
      let b = Black
      let e = Empty
      let n (c, l, kv, r) = Node (c, kv, l, r)
      let impl =
        function
        | (B, N (R, N (R, s, x, t), y, u), z, v)
        | (B, N (R, s, x, N (R, t, y, u)), z, v)
        | (B, s, x, N (R, N (R, t, y, u), z, v))
        | (B, s, x, N (R, t, y, N (R, u, z, v))) ->
          (r, n (b, s, x, t), y, n (b, u, z, v))
        | body -> body
      fun t ->
        let (c', l', kv', r') = impl t
        (c', kv', l', r')

    let insert k v =
      let rec loop =
        function
        | Empty ->
          (Red, (k, v), Empty, Empty)
        | Node (color, (k', v'), left, right) ->
          let comparison = compare k k'
          if comparison < 0 then
            balance (color, Node (loop left), (k', v'), right)
          else if comparison > 0 then
            balance (color, left, (k', v'), Node (loop right))
          else
            (color, (k', v'), left, right)
      fun tree ->
        let (color, kv', left, right) = tree |> loop
        Node (Black, kv', left, right)

    let signature =
      {
        Empty           = empty
        Insert          = insert
        TryFind         = tryFind
      }
