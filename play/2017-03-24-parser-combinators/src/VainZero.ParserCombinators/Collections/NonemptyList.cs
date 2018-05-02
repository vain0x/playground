using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;

namespace VainZero.Collections
{
    public struct NonemptyList<TValue>
        : IReadOnlyList<TValue>
    {
        public TValue First { get; }
        public IReadOnlyList<TValue> Rest { get; }

        public int Count =>
            Rest.Count + 1;

        public TValue this[int index] =>
            index == 0
            ? First
            : Rest[index - 1];

        public IEnumerator<TValue> GetEnumerator()
        {
            yield return First;
            foreach (var x in Rest)
            {
                yield return x;
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        internal NonemptyList(TValue first, IReadOnlyList<TValue> rest)
        {
            First = first;
            Rest = rest;
        }
    }

    public static class NonemptyListModule
    {
        public static NonemptyList<X>
            Create<X>(X first, IReadOnlyList<X> rest)
        {
            return new NonemptyList<X>(first, rest);
        }
    }
}
