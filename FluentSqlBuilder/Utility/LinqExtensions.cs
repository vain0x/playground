using System;
using System.Collections.Generic;
using System.Linq;
using Optional;
using Optional.Unsafe;

namespace FluentSqlBuilder
{
    static class LinqExtensions
    {
        public static bool IsEmpty<X>(this IEnumerable<X> xs) =>
            !xs.Any();

        public static IEnumerable<X> Concat<X>(this IEnumerable<IEnumerable<X>> xxs) =>
            xxs.SelectMany(xs => xs);

        /// <summary>
        /// シーケンスの各要素の間に separator を挟んだシーケンスを取得する。
        /// </summary>
        public static IEnumerable<X> Intersperse<X>(this IEnumerable<X> xs, X separator)
        {
            var isFirst = true;
            foreach (var x in xs)
            {
                if (isFirst)
                {
                    isFirst = false;
                }
                else
                {
                    yield return separator;
                }

                yield return x;
            }
        }

        public static IEnumerable<X> Intercalate<X>(
            this IEnumerable<IEnumerable<X>> xxs,
            IEnumerable<X> separator
        ) =>
            xxs.Intersperse(separator).Concat();

        public static string Intercalate(
            this IEnumerable<string> ss,
            char separator
        )
        {
            return string.Join(separator.ToString(), ss);
        }

        public static IEnumerable<X> Enclose<X>(this IEnumerable<X> xs, X left, X right)
        {
            yield return left;
            foreach(var x in xs)
            {
                yield return x;
            }
            yield return right;
        }

        public static bool IsSingle<X>(this IEnumerable<X> xs, X x) =>
            xs.Any() && !xs.Skip(1).Any() && Equals(xs.First(), x);

        public static IEnumerable<Y> Choose<X, Y>(this IEnumerable<X> xs, Func<X, Option<Y>> f)
        {
            foreach (var x in xs)
            {
                var y = f(x);
                if (y.HasValue) yield return y.ValueOrFailure();
            }
        }

        public static Option<V> GetValueOrNone<K, V>(this IReadOnlyDictionary<K, V> dictionary, K key)
        {
            V value;
            return dictionary.TryGetValue(key, out value) ? value.Some() : value.None();
        }
    }
}
