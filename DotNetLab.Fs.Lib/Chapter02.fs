namespace DotNetLab.Fs.Lib.PFDS

module Chapter02 =
  // Ex2.1
  let rec suffix =
    function
    | x :: xs ->
      (x :: xs) :: suffix xs
    | [] ->
      [[]]

  (*
    時間計算量 O(n):
        長さ n のリスト xs について、time(suffix xs) = T(n) とおく。
        任意の xs: 'x list をとる。n = length xs とおく。
        xs = [] のとき、
            T(0) = time(suffix []) = 1
        xs = x :: xs' のとき、
            T(n) = time(suffix xs)
                = time((x :: xs') :: suffix xs')
                = time(suffix xs') + 2
                = T(n - 1) + 2
        従って、T(n) = 2n + 1 = O(n)
    空間計算量 O(n):
        長さ n のリスト xs について、space(suffix xs) = S(n) とおく。
        任意の xs: 'x list をとる。n = length xs とおく。
        xs = [] のとき
            S(0) = space([[]]) = 1
        xs = x :: xs' のとき、xs' は xs の部分リストなので空間 O(1) で表現できて、
            S(n) = space(suffix xs)
                = space(x :: xs') + space(suffix xs')
                = space(suffix xs') + O(1)
        従って、S(n) = O(n)
  *)

  type ISet<'e, 's> =
    abstract member Empty: 's
    abstract member Insert: 'e -> 's
    abstract member Contains: 'e -> bool

  /// Unbalanced binary search tree representing set.
  type BinarySearchTree<'e when 'e: comparison> =
    | Empty
    | Node of BinarySearchTree<'e> * 'e * BinarySearchTree<'e>
  with
    member this.Insert(x) =
      match this with
      | Empty -> Node (Empty, x, Empty)
      | Node (left, y, right) ->
        if x < y then
          Node (left.Insert(x), y, right)
        elif x > y then
          Node (left, y, right.Insert(x))
        else
          this

    member this.Contains(x) =
      match this with
      | Empty -> false
      | Node (left, y, right) ->
        if x < y then
          left.Contains(x)
        elif x > y then
          right.Contains(x)
        else true

    interface ISet<'e, BinarySearchTree<'e>> with
      override this.Empty = Empty
      override this.Insert(x) = this.Insert(x)
      override this.Contains(x) = this.Contains(x)
