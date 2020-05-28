using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace CSharpObjectDiff
{
    /// <summary>
    /// リストの変更の種類
    /// </summary>
    public enum ListDeltaKind
    {
        Add,
        Remove,
    }

    /// <summary>
    /// リストの一要素の変更を表す。
    /// </summary>
    public struct ListDelta
        : IEquatable<ListDelta>
    {
        public readonly ListDeltaKind Kind;

        /// <summary>
        /// 関連する要素の変更前のリストにおける位置
        /// </summary>
        public readonly int OldIndex;

        /// <summary>
        /// 関連する要素の変更後のリストにおける位置
        /// </summary>
        public readonly int NewIndex;

        #region Equality

        public override bool Equals(object obj)
        {
            return obj is ListDelta && Equals((ListDelta)obj);
        }

        public bool Equals(ListDelta other)
        {
            return Kind == other.Kind
                && OldIndex == other.OldIndex
                && NewIndex == other.NewIndex;
        }

        public override int GetHashCode()
        {
            return ValueTuple.Create(Kind, OldIndex, NewIndex).GetHashCode();
        }

        public static bool operator ==(ListDelta l, ListDelta r)
        {
            return l.Equals(r);
        }

        public static bool operator !=(ListDelta l, ListDelta r)
        {
            return !(l == r);
        }

        #endregion

        public ListDelta(ListDeltaKind kind, int oldIndex, int newIndex)
        {
            Kind = kind;
            OldIndex = oldIndex;
            NewIndex = newIndex;
        }

        public static ListDelta CreateAdd(int oldIndex, int newIndex)
        {
            return new ListDelta(ListDeltaKind.Add, oldIndex, newIndex);
        }

        public static ListDelta CreateRemove(int oldIndex, int newIndex)
        {
            return new ListDelta(ListDeltaKind.Remove, oldIndex, newIndex);
        }
    }

    /// <summary>
    /// リストの差分を計算するもの
    ///
    /// (要素の移動に対応していない、Remove の効率が悪い、などの問題がある。)
    /// </summary>
    sealed class ListDiffer<T>
    {
        readonly IReadOnlyList<T> _oldItems;
        readonly IReadOnlyList<T> _newItems;
        readonly IEqualityComparer<T> _comparer;

        bool Eq(T l, T r)
        {
            return _comparer.Equals(l, r);
        }

        internal IEnumerable<ListDelta> GetEnumerable()
        {
            // 変更前のリストのインデックス
            var si = 0;

            // 変更後のリストのインデックス
            var ti = 0;

            var sn = _oldItems.Count;
            var tn = _newItems.Count;

            while (si < sn || ti < tn)
            {
                // この時点で元になるコレクションは以下の状態になっている:
                //    newItems.Take(ti).Concat(oldItems.Skip(si))

                // old  new
                //   y    x  <- y は削除されたとみなす
                //   x    :
                //   :
                if (ti == tn || (si + 1 < sn && Eq(_oldItems[si + 1], _newItems[ti])))
                {
                    yield return ListDelta.CreateRemove(si, ti);
                    si++;
                    continue;
                }

                // old  new
                //   x    y  <- y は追加されたとみなす
                //   :    x
                //        :
                if (si == sn || (ti + 1 < tn && Eq(_oldItems[si], _newItems[ti + 1])))
                {
                    yield return ListDelta.CreateAdd(si, ti);
                    ti++;
                    continue;
                }

                if (!Eq(_oldItems[si], _newItems[ti]))
                {
                    yield return ListDelta.CreateRemove(si, ti);
                    si++;

                    yield return ListDelta.CreateAdd(si, ti);
                    ti++;
                    continue;
                }

                si++;
                ti++;
            }
        }

        public ListDiffer(IReadOnlyList<T> oldItems, IReadOnlyList<T> newItems, IEqualityComparer<T> comparer)
        {
            _oldItems = oldItems;
            _newItems = newItems;
            _comparer = comparer;
        }
    }

    public static class ListDifferModule
    {
        /// <summary>
        /// 2つのリストの差分を計算する。
        /// </summary>
        public static IEnumerable<ListDelta> Diff<T>(IReadOnlyList<T> oldList, IReadOnlyList<T> newList, IEqualityComparer<T> comparer = null)
        {
            return new ListDiffer<T>(oldList, newList, comparer ?? EqualityComparer<T>.Default).GetEnumerable();
        }
    }

    public sealed class PatchableList<T>
        : IEnumerable<T>
    {
        readonly List<T> _list = new List<T>();

        public void Patch(IReadOnlyList<T> newList)
        {
            var oldList = _list.ToArray();
            var diff = ListDifferModule.Diff<T>(oldList, newList);

            foreach (var delta in diff)
            {
                switch (delta.Kind)
                {
                    case ListDeltaKind.Add:
                        Console.WriteLine("Add " + newList[delta.NewIndex]);
                        _list.Add(newList[delta.NewIndex]);
                        break;
                    case ListDeltaKind.Remove:
                        Console.WriteLine("Remove " + oldList[delta.OldIndex]);
                        _list.RemoveAt(delta.NewIndex);
                        break;
                    default:
                        throw new NotSupportedException();
                }
            }
        }

        public IEnumerator<T> GetEnumerator()
        {
            return _list.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }

    public sealed class ListDifferTest
    {
        public void Test()
        {
            var list = new PatchableList<string>();
            var patch = new Action<string[]>(expected =>
            {
                list.Patch(expected);
                list.ToArray().Is(expected);
            });

            patch(new[] { "a", "b" });
            patch(new[] { "a", "b", "c" });
            patch(new[] { "a", "c" });
            patch(new[] { "c", "b", "a" });
        }
    }

    public static class Extensions
    {
        public static void Is<T>(this T[] actual, T[] expected)
        {
            if (!actual.SequenceEqual(expected))
            {
                Console.WriteLine("actual = {0}, expected = {1}", actual, expected);
                throw new Exception("Assertion violated");
            }
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            new ListDifferTest().Test();
            Console.WriteLine("Hello World!");
        }
    }
}
