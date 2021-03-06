﻿using System;
using System.Collections.Generic;
using System.Linq;
using DotNetKit.ErrorHandling;

namespace AsterSql
{
    static class EnumerableExtension
    {
        static IEnumerable<X> Enumerate<X>(IEnumerator<X> enumerator)
        {
            while (enumerator.MoveNext())
            {
                yield return enumerator.Current;
            }
        }

        public static bool IsEmpty<X>(this IEnumerable<X> xs)
        {
            return !xs.Any();
        }

        public static IEnumerable<X> Concat<X>(this IEnumerable<IEnumerable<X>> xxs)
        {
            return xxs.SelectMany(xs => xs);
        }

        public static Option<Tuple<X, IEnumerable<X>>> UnconsOrNone<X>(this IEnumerable<X> xs)
        {
            using (var enumerator = xs.GetEnumerator())
            {
                if (!enumerator.MoveNext())
                {
                    return Option.None;
                }

                return Option.Some(Tuple.Create(enumerator.Current, Enumerate(enumerator)));
            }
        }

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
        )
        {
            return xxs.Intersperse(separator).Concat();
        }

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

        public static bool IsSingle<X>(this IEnumerable<X> xs, X x)
        {
            return xs.Any() && !xs.Skip(1).Any() && Equals(xs.First(), x);
        }
    }
}
