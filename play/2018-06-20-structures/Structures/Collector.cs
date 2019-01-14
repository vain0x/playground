using System;
using System.Collections.Generic;
using System.Linq;

namespace Structures
{
    using Nongeneric = System.Collections;

    public interface ICollector<T, TState, TResult>
    {
        TState Create(T item);
        TState Add(TState state, T item);
        TResult Complete(TState state);
    }

    public static class EnumerableExtension
    {
        public static IEnumerable<IGrouping<TKey, TValue>> GroupByPoor<T, TKey, TValue>(
            this IEnumerable<KeyValuePair<TKey, TValue>> source
        )
        {
            var groups = new Dictionary<TKey, List<TValue>>();

            foreach (var item in source)
            {
                if (groups.TryGetValue(item.Key, out var group))
                {
                    group.Add(item.Value);
                }
                else
                {
                    groups.Add(item.Key, new List<TValue>() { item.Value });
                }
            }

            foreach (var kv in groups)
            {
                yield return new Grouping<TKey, TValue> { Key = kv.Key, Values = kv.Value.ToArray() };
            }
        }

        public static IEnumerable<TGroup> GroupByGeneric<T, TKey, TValue, TGroupState, TGroup>(
            this IEnumerable<KeyValuePair<TKey, TValue>> source,
            ICollector<KeyValuePair<TKey, TValue>, TGroupState, TGroup> collector
        )
        {
            var groups = new Dictionary<TKey, TGroupState>();

            foreach (var item in source)
            {
                if (!groups.TryGetValue(item.Key, out var group))
                {
                    groups[item.Key] = collector.Add(group, item);
                }
                else
                {
                    groups.Add(item.Key, collector.Create(item));
                }
            }

            foreach (var kv in groups)
            {
                yield return collector.Complete(kv.Value);
            }
        }
    }

    public struct ToGroupingCollector<TKey, TValue>
        : ICollector<
            KeyValuePair<TKey, TValue>,
            ToGroupingCollector<TKey, TValue>.Builder,
            IGrouping<TKey, TValue>
        >
    {
        public Builder Create(KeyValuePair<TKey, TValue> item) =>
            new Builder() { Key = item.Key, Values = new List<TValue> { item.Value } };

        public Builder Add(Builder builder, KeyValuePair<TKey, TValue> item)
        {
            builder.Values.Add(item.Value);
            return builder;
        }

        public IGrouping<TKey, TValue> Complete(Builder builder) =>
            new Grouping<TKey, TValue> { Key = builder.Key, Values = builder.Values.ToArray() };

        public sealed class Builder
        {
            public TKey Key { get; set; }
            public List<TValue> Values { get; set; }
        }
    }

    public class Grouping<TKey, T>
        : IGrouping<TKey, T>
    {
        public TKey Key { get; set; }
        public T[] Values { get; set; }

        public IEnumerator<T> GetEnumerator() =>
            ((IEnumerable<T>)Values).GetEnumerator();

        Nongeneric.IEnumerator Nongeneric.IEnumerable.GetEnumerator() =>
            GetEnumerator();
    }
}
