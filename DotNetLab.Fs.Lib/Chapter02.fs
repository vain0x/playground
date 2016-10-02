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
