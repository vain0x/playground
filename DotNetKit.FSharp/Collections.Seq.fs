namespace DotNetKit.FSharp

open System.Collections.Generic

[<RequireQualifiedAccess>]
module Seq =
  /// <summary>
  /// Applies f for each element in xs and partition them into two list.
  /// The first is y's where f x = Some y
  /// and the other is x's where f x = None.
  /// <para lang="ja">
  /// シーケンスの各要素に関数を適用し、2つのリストに分割する。
  /// 1つ目のリストは f x = Some y となる y からなり、
  /// 2つ目のリストは f x = None となる x からなる。
  /// </para>
  /// </summary>
  let paritionMap
    (f: 'x -> option<'y>)
    (xs: seq<'x>)
    : IReadOnlyList<'y> * IReadOnlyList<'x>
    =
    let ys = ResizeArray()
    let rest = ResizeArray()
    for x in xs do
      match f x with
      | Some y ->
        ys.Add(y)
      | None ->
        rest.Add(x)
    (ys :> IReadOnlyList<_>, rest :> IReadOnlyList<_>)
