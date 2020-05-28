using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace DictionaryBench
{
    public sealed class LinearMap<TKey, TValue>
        : IReadOnlyDictionary<TKey, TValue>
        , IDictionary<TKey, TValue>
    {
        readonly KeyValuePair<TKey, TValue>[] _buffer;
        int _cursor = 0;

        static readonly IEqualityComparer<TKey> _comparer = EqualityComparer<TKey>.Default;

        public LinearMap(KeyValuePair<TKey, TValue>[] buffer) => _buffer = buffer;

        public TValue this[TKey key]
        {
            get =>
                TryGetValue(key, out var value)
                    ? value
                    : throw new ArgumentException("key");
            set =>
                throw new NotSupportedException();
        }

        public ICollection<TKey> Keys =>
            throw new NotImplementedException();

        public ICollection<TValue> Values =>
            throw new NotImplementedException();

        public bool ContainsKey(TKey key) =>
            TryGetValue(key, out var value);

        public bool TryGetValue(TKey key, out TValue value)
        {
            var cursor = _cursor;
            var start = cursor;

            while (true)
            {
                if (_comparer.Equals(key, _buffer[cursor].Key))
                {
                    _cursor = cursor;
                    value = _buffer[cursor].Value;
                    return true;
                }

                cursor = cursor + 1 == _buffer.Length ? 0 : cursor + 1;

                if (cursor == start)
                {
                    value = default(TValue);
                    return false;
                }
            }
        }

        void IDictionary<TKey, TValue>.Add(TKey key, TValue value) =>
            throw new NotSupportedException();

        bool IDictionary<TKey, TValue>.Remove(TKey key) =>
            throw new NotSupportedException();

        public int Count =>
            _buffer.Length;

        bool ICollection<KeyValuePair<TKey, TValue>>.IsReadOnly =>
            true;

        void ICollection<KeyValuePair<TKey, TValue>>.Add(KeyValuePair<TKey, TValue> item) =>
            throw new NotSupportedException();

        bool ICollection<KeyValuePair<TKey, TValue>>.Remove(KeyValuePair<TKey, TValue> item) =>
            throw new NotSupportedException();

        void ICollection<KeyValuePair<TKey, TValue>>.Clear() =>
            throw new NotSupportedException();

        bool ICollection<KeyValuePair<TKey, TValue>>.Contains(KeyValuePair<TKey, TValue> item) =>
            throw new NotImplementedException();

        void ICollection<KeyValuePair<TKey, TValue>>.CopyTo(KeyValuePair<TKey, TValue>[] array, int arrayIndex) =>
            throw new NotImplementedException();

        IEnumerable<TKey> IReadOnlyDictionary<TKey, TValue>.Keys =>
            Keys;

        IEnumerable<TValue> IReadOnlyDictionary<TKey, TValue>.Values =>
            Values;

        public IEnumerator<KeyValuePair<TKey, TValue>> GetEnumerator() =>
            ((IEnumerable<KeyValuePair<TKey, TValue>>)_buffer).GetEnumerator();

        IEnumerator IEnumerable.GetEnumerator() =>
            GetEnumerator();
    }

    public static class LinearMap
    {
        public static LinearMap<TKey, T> ToLinearMap<T, TKey>(this IEnumerable<T> source, Func<T, TKey> keySelector)
        {
            var a = (T[])source;
            var buffer = new KeyValuePair<TKey, T>[a.Length];
            for (var i = 0; i < a.Length; i++)
            {
                var key = keySelector(a[i]);
                buffer[i] = new KeyValuePair<TKey, T>(key, a[i]);
            }
            return new LinearMap<TKey, T>(buffer);
        }
    }
}
