using System;
using System.Collections.Generic;
using Nongeneric = System.Collections;

namespace Structures
{
    /// <summary>
    /// Represents an immutable associative list.
    /// </summary>
    public sealed class Assoc<TKey, TValue>
        : IEnumerable<KeyValuePair<TKey, TValue>>
    {
        TKey Key { get; }
        TValue Value { get; }

        /// <summary>
        /// Gets the number of key-value pairs in this. O(1) time.
        /// </summary>
        public int Count { get; }

        /// <summary>
        /// Equals <c>this</c> if empty.
        /// </summary>
        Assoc<TKey, TValue> Tail { get; }

        Assoc()
        {
            Key = default(TKey);
            Value = default(TValue);
            Count = 0;
            Tail = this;
        }

        public Assoc(TKey key, TValue value, Assoc<TKey, TValue> tail)
        {
            Key = key;
            Value = value;
            Count = tail.Count + 1;
            Tail = tail;
        }

        internal static Assoc<TKey, TValue> Empty { get; } =
            new Assoc<TKey, TValue>();

        public bool IsEmpty => Count == 0;

        internal bool TryFind(TKey targetKey, out TValue value)
        {
            var assoc = this;
            while (!IsEmpty)
            {
                if (EqualityComparer<TKey>.Default.Equals(targetKey, assoc.Key))
                {
                    value = assoc.Value;
                    return true;
                }
                assoc = assoc.Tail;
            }
            value = default(TValue);
            return false;
        }

        internal Assoc<TKey, TValue> SetOrAdd(TKey newKey, TValue newValue, out int countChanged)
        {
            bool removed;
            TValue oldValue;
            var it = TryRemove(newKey, out removed, out oldValue);
            countChanged = removed ? 0 : 1;
            return new Assoc<TKey, TValue>(newKey, newValue, it);
        }

        internal Assoc<TKey, TValue> TryRemove(TKey targetKey, out bool removed, out TValue value)
        {
            if (IsEmpty)
            {
                removed = false;
                value = default(TValue);
                return this;
            }

            if (EqualityComparer<TKey>.Default.Equals(targetKey, Key))
            {
                removed = true;
                value = Value;
                return Tail;
            }

            var newTail = Tail.TryRemove(targetKey, out removed, out value);
            return removed ? new Assoc<TKey, TValue>(Key, Value, newTail) : this;
        }

        public IEnumerator<KeyValuePair<TKey, TValue>> GetEnumerator()
        {
            var assoc = this;
            while (!assoc.IsEmpty)
            {
                yield return new KeyValuePair<TKey, TValue>(assoc.Key, assoc.Value);
                assoc = assoc.Tail;
            }
        }

        Nongeneric.IEnumerator Nongeneric.IEnumerable.GetEnumerator() =>
            GetEnumerator();
    }

    public static class Assoc
    {
        public static Assoc<K, V> Empty<K, V>() => Assoc<K, V>.Empty;

        public static Assoc<K, V> Singleton<K, V>(K key, V value) =>
            Empty<K, V>().SetOrAdd(key, value, out var _);
    }
}
