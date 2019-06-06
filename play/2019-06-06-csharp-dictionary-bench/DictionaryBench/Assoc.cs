using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace DictionaryBench
{
    public sealed class Assoc<TKey, TValue>
        : IReadOnlyDictionary<TKey, TValue>
        , IDictionary<TKey, TValue>
    {
        readonly Dictionary<TKey, TValue> _inner;

        internal Assoc(Dictionary<TKey, TValue> inner)
        {
            _inner = inner;
        }

        public TValue this[TKey key]
        {
            get
            {
                return _inner[key];
            }
            set
            {
                throw new NotSupportedException();
            }
        }

        public ICollection<TKey> Keys
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        public ICollection<TValue> Values
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        public bool ContainsKey(TKey key)
        {
            return _inner.ContainsKey(key);
        }

        public bool TryGetValue(TKey key, out TValue value)
        {
            return _inner.TryGetValue(key, out value);
        }

        void IDictionary<TKey, TValue>.Add(TKey key, TValue value)
        {
            throw new NotSupportedException();
        }

        bool IDictionary<TKey, TValue>.Remove(TKey key)
        {
            throw new NotSupportedException();
        }

        public int Count
        {
            get
            {
                return _inner.Count;
            }
        }

        bool ICollection<KeyValuePair<TKey, TValue>>.IsReadOnly
        {
            get
            {
                return true;
            }
        }

        void ICollection<KeyValuePair<TKey, TValue>>.Add(KeyValuePair<TKey, TValue> item)
        {
            throw new NotSupportedException();
        }

        bool ICollection<KeyValuePair<TKey, TValue>>.Remove(KeyValuePair<TKey, TValue> item)
        {
            throw new NotSupportedException();
        }

        void ICollection<KeyValuePair<TKey, TValue>>.Clear()
        {
            throw new NotSupportedException();
        }

        public bool Contains(KeyValuePair<TKey, TValue> item)
        {
            return _inner.Contains(item);
        }

        public void CopyTo(KeyValuePair<TKey, TValue>[] array, int arrayIndex)
        {
            ((ICollection<KeyValuePair<TKey, TValue>>)_inner).CopyTo(array, arrayIndex);
        }

        IEnumerable<TKey> IReadOnlyDictionary<TKey, TValue>.Keys
        {
            get
            {
                return _inner.Keys;
            }
        }

        IEnumerable<TValue> IReadOnlyDictionary<TKey, TValue>.Values
        {
            get
            {
                return _inner.Values;
            }
        }

        Dictionary<TKey, TValue>.Enumerator GetEnumerator()
        {
            return _inner.GetEnumerator();
        }

        IEnumerator<KeyValuePair<TKey, TValue>> IEnumerable<KeyValuePair<TKey, TValue>>.GetEnumerator()
        {
            return _inner.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return _inner.GetEnumerator();
        }
    }

    public static class Assoc
    {
        public static Assoc<TKey, T> ToAssoc<T, TKey>(this IEnumerable<T> source, Func<T, TKey> keySelector)
        {
            return new Assoc<TKey, T>(source.ToDictionary(keySelector));
        }
    }
}
