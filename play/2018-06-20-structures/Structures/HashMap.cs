using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using Nongeneric = System.Collections;

namespace Structures
{
    public sealed class HashMap<TKey, TValue>
        : IEnumerable<KeyValuePair<TKey, TValue>>
    {
        AvlTree<Assoc<TKey, TValue>>.Node Root { get; }

        public int Count { get; }

        HashMap(
            AvlTree<Assoc<TKey, TValue>>.Node root,
            int count
        )
        {
            Root = root;
            Count = count;
        }

        internal static HashMap<TKey, TValue> Empty { get; } =
            new HashMap<TKey, TValue>(
                AvlTree<Assoc<TKey, TValue>>.Node.Empty,
                0
            );

        HashMap<TKey, TValue> SetRoot(AvlTree<Assoc<TKey, TValue>>.Node root)
        {
            if (root.IsEmpty) return Empty;

            return new HashMap<TKey, TValue>(root, Count);
        }

        public bool TryFind(TKey key, out TValue value)
        {
            Assoc<TKey, TValue> assoc;
            if (!Root.TryFind(key.GetHashCode(), out assoc))
            {
                value = default(TValue);
                return false;
            }

            return assoc.TryFind(key, out value);
        }

        public HashMap<TKey, TValue> SetOrAdd(TKey key, TValue value)
        {
            bool mutated;
            var modifier = new SetOrAddEntryModifier() { Key = key, Value = value };
            var newRoot = Root.ModifyEntry(key.GetHashCode(), modifier, out mutated);
            if (!mutated) return this;

            return new HashMap<TKey, TValue>(newRoot, Count + modifier.CountChanged);
        }

        public HashMap<TKey, TValue> TryRemove(TKey key, out bool removed, out TValue value)
        {
            var modifier = new TryRemoveEntryModifier() { Key = key };
            var newRoot = Root.ModifyEntry(key.GetHashCode(), modifier, out removed);

            value = modifier.Value;

            if (!removed) return this;
            return new HashMap<TKey, TValue>(newRoot, Count + modifier.CountChanged);
        }

        public IEnumerator<KeyValuePair<TKey, TValue>> GetEnumerator() =>
            Root.ToEnumerable().SelectMany(kvs => kvs).GetEnumerator();

        Nongeneric.IEnumerator Nongeneric.IEnumerable.GetEnumerator() =>
            GetEnumerator();

        sealed class SetOrAddEntryModifier
            : AvlTree<Assoc<TKey, TValue>>.IEntryModifier
        {
            public TKey Key;
            public TValue Value;
            public int CountChanged;

            public Assoc<TKey, TValue> Create(out bool mutated)
            {
                mutated = true;
                return Assoc.Empty<TKey, TValue>().SetOrAdd(Key, Value, out CountChanged);
            }

            public Assoc<TKey, TValue> Update(Assoc<TKey, TValue> source, out bool replaced, out bool mutated)
            {
                replaced = true;
                mutated = true;
                return source.SetOrAdd(Key, Value, out CountChanged);
            }
        }

        sealed class TryRemoveEntryModifier
            : AvlTree<Assoc<TKey, TValue>>.IEntryModifier
        {
            public TKey Key;
            public TValue Value;
            public int CountChanged;

            public Assoc<TKey, TValue> Create(out bool mutated)
            {
                mutated = false;
                return default(Assoc<TKey, TValue>);
            }

            public Assoc<TKey, TValue> Update(Assoc<TKey, TValue> source, out bool replaced, out bool mutated)
            {
                var newAssoc = source.TryRemove(Key, out mutated, out Value);
                if (!mutated)
                {
                    replaced = false;
                    return default(Assoc<TKey, TValue>);
                }

                CountChanged--;
                replaced = !newAssoc.IsEmpty;
                return newAssoc;
            }
        }
    }

    public static class HashMap
    {
        public static HashMap<K, V> Empty<K, V>() =>
            HashMap<K, V>.Empty;

        public static HashMap<K, V> ToHashMap<T, K, V>(
            this IEnumerable<KeyValuePair<K, V>> source
        ) =>
            source.ToHashMap(kv => kv.Key, kv => kv.Value);

        public static HashMap<K, V> ToHashMap<T, K, V>(
            this IEnumerable<T> source,
            Func<T, K> keySelector,
            Func<T, V> valueSelector
        )
        {
            var map = Empty<K, V>();
            foreach (var item in source)
            {
                map = map.SetOrAdd(keySelector(item), valueSelector(item));
            }
            return map;
        }
    }
}
