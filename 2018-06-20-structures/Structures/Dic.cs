using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Structures
{
    /// <summary>
    /// イミュータブルな辞書を表す。
    ///
    /// <para>
    /// <see cref="Dictionary{TKey, TValue}" /> と同様。ただし Add などの変更操作はすべて拒否するため、内容が変化することはない。
    /// </para>
    ///
    /// <para>
    /// <c>ImmutableDictionary</c> と類似している。
    /// <c>Dic</c> は要素単位での変更 (Add など) ができない代わりに、性能が高い。
    /// </para>
    /// </summary>
    [DebuggerDisplay("Count = {Count}")]
    public sealed class Dic<TKey, TValue>
        : IEnumerable
        , IEnumerable<KeyValuePair<TKey, TValue>>
        , IReadOnlyCollection<KeyValuePair<TKey, TValue>>
        , ICollection
        , ICollection<KeyValuePair<TKey, TValue>>
        , IReadOnlyDictionary<TKey, TValue>
        , IDictionary
        , IDictionary<TKey, TValue>
    {
        readonly Dictionary<TKey, TValue> _inner;

        Dic(Dictionary<TKey, TValue> inner)
        {
            Debug.Assert(inner != null);
            _inner = inner;
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        internal static readonly Dic<TKey, TValue> _empty =
            new Dic<TKey, TValue>(new Dictionary<TKey, TValue>());

        internal static Dic<TKey, TValue> FromDictionary(Dictionary<TKey, TValue> inner)
        {
            if (inner.Count == 0)
                return _empty;

            return new Dic<TKey, TValue>(inner);
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

        public IReadOnlyCollection<TKey> Keys
        {
            get
            {
                return _inner.Keys;
            }
        }

        public IReadOnlyCollection<TValue> Values
        {
            get
            {
                return _inner.Values;
            }
        }

        /// <summary>
        /// 要素数を取得する。
        /// </summary>
        public int Count
        {
            get
            {
                return _inner.Count;
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

        public bool Contains(KeyValuePair<TKey, TValue> item)
        {
            return _inner.Contains(item);
        }

        public void CopyTo(KeyValuePair<TKey, TValue>[] array, int arrayIndex)
        {
            ((ICollection<KeyValuePair<TKey, TValue>>)_inner).CopyTo(array, arrayIndex);
        }

        public Dictionary<TKey, TValue>.Enumerator GetEnumerator()
        {
            return _inner.GetEnumerator();
        }

        #region Interfaces

        // インターフェイスの実装。
        // 変更操作は NotSupportedException を送出する。読み取り操作は _inner の同じメソッドに委譲する。

        void IDictionary<TKey, TValue>.Add(TKey key, TValue value)
        {
            throw new NotSupportedException();
        }

        bool IDictionary<TKey, TValue>.Remove(TKey key)
        {
            throw new NotSupportedException();
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

        ICollection<TKey> IDictionary<TKey, TValue>.Keys
        {
            get
            {
                return _inner.Keys;
            }
        }

        ICollection<TValue> IDictionary<TKey, TValue>.Values
        {
            get
            {
                return _inner.Values;
            }
        }

        ICollection IDictionary.Keys
        {
            get
            {
                return ((IDictionary)_inner).Keys;
            }
        }

        ICollection IDictionary.Values
        {
            get
            {
                return ((IDictionary)_inner).Values;
            }
        }

        bool IDictionary.IsReadOnly
        {
            get
            {
                return true;
            }
        }

        bool IDictionary.IsFixedSize
        {
            get
            {
                return true;
            }
        }

        object ICollection.SyncRoot
        {
            get
            {
                throw new NotSupportedException();
            }
        }

        bool ICollection.IsSynchronized
        {
            get
            {
                return true;
            }
        }

        object IDictionary.this[object key]
        {
            get
            {
                return ((IDictionary)_inner)[key];
            }
            set
            {
                throw new NotSupportedException();
            }
        }

        IEnumerator<KeyValuePair<TKey, TValue>> IEnumerable<KeyValuePair<TKey, TValue>>.GetEnumerator()
        {
            return _inner.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return _inner.GetEnumerator();
        }

        bool IDictionary.Contains(object key)
        {
            return ((IDictionary)_inner).Contains(key);
        }

        void IDictionary.Add(object key, object value)
        {
            throw new NotSupportedException();
        }

        void IDictionary.Clear()
        {
            throw new NotSupportedException();
        }

        IDictionaryEnumerator IDictionary.GetEnumerator()
        {
            return ((IDictionary)_inner).GetEnumerator();
        }

        void IDictionary.Remove(object key)
        {
            throw new NotSupportedException();
        }

        void ICollection.CopyTo(Array array, int index)
        {
            ((IDictionary)_inner).CopyTo(array, index);
        }

        #endregion
    }

    /// <summary>
    /// 書き込み用の辞書を表す。イミュータブルな辞書 Dic に効率的に変換できる。
    /// </summary>
    public sealed class DicBuilder<TKey, TValue>
    {
        Dictionary<TKey, TValue> _inner;

        [Obsolete("Dic.CreateBuilder<TKey, TValue>() を使用してください。")]
        public DicBuilder()
        {
            _inner = new Dictionary<TKey, TValue>();
        }

        internal DicBuilder(Dictionary<TKey, TValue> inner)
        {
            Debug.Assert(inner != null);
            _inner = inner;
        }

        public void Add(TKey key, TValue value)
        {
            _inner.Add(key, value);
        }

        public void Remove(TKey key)
        {
            _inner.Remove(key);
        }

        public void Clear()
        {
            _inner.Clear();
        }

        public bool ContainsKey(TKey key)
        {
            return _inner.ContainsKey(key);
        }

        public bool TryGetValue(TKey key, out TValue value)
        {
            return _inner.TryGetValue(key, out value);
        }

        /// <summary>
        /// イミュータブルな辞書を生成する。
        ///
        /// <para>
        /// このオブジェクトは使用できなくなる。
        /// </para>
        /// </summary>
        public Dic<TKey, TValue> MoveToImmutable()
        {
            if (_inner == null)
                throw new InvalidOperationException();

            var dic = Dic<TKey, TValue>.FromDictionary(_inner);
            _inner = null;
            return dic;
        }

        /// <summary>
        /// イミュータブルな辞書を生成する。
        ///
        /// <para>
        /// この操作の後にこのオブジェクトを使用しないなら、<c>MoveToImmutable</c> の方が効率的。
        /// </para>
        /// </summary>
        public Dic<TKey, TValue> ToImmutable()
        {
            if (_inner == null)
                throw new InvalidOperationException();

            return Dic<TKey, TValue>.FromDictionary(new Dictionary<TKey, TValue>(_inner, _inner.Comparer));
        }
    }

    public static class Dic
    {
        /// <summary>
        /// 空のイミュータブルな辞書を取得する。
        /// </summary>
        public static Dic<TKey, TValue> Empty<TKey, TValue>()
        {
            return Dic<TKey, TValue>._empty;
        }

        /// <summary>
        /// キーと値のペアの列をイミュータブルな辞書に変換する。
        /// </summary>
        /// <exception cref="ArgumentException">
        /// キーが重複しているとき。
        /// </exception>
        public static Dic<TKey, TValue> ToDic<TKey, TValue>(this IEnumerable<KeyValuePair<TKey, TValue>> source, IEqualityComparer<TKey> keyComparer = null)
        {
            return Dic<TKey, TValue>.FromDictionary(source.ToDictionary(kv => kv.Key, kv => kv.Value, keyComparer));
        }

        /// <summary>
        /// イミュータブルな辞書を生成する。
        ///
        /// <para>
        /// 各要素 <c>item</c> に対して <c>keySelector(item)</c> をキーとする辞書を生成する。
        /// </para>
        /// </summary>
        /// <exception cref="ArgumentException">
        /// キーが重複しているとき。
        /// </exception>
        public static Dic<TKey, T> ToDic<T, TKey>(this IEnumerable<T> source, Func<T, TKey> keySelector, IEqualityComparer<TKey> keyComparer = null)
        {
            return Dic<TKey, T>.FromDictionary(source.ToDictionary(keySelector, keyComparer));
        }

        /// <summary>
        /// イミュータブルな辞書を生成する。
        ///
        /// <para>
        /// 各要素 <c>item</c> に対して <c>keySelector(item)</c> をキーとし、<c>elementSelector(item)</c> を値とする辞書を生成する。
        /// </para>
        /// </summary>
        /// <exception cref="ArgumentException">
        /// キーが重複しているとき。
        /// </exception>
        public static Dic<TKey, TValue> ToDic<T, TKey, TValue>(this IEnumerable<T> source, Func<T, TKey> keySelector, Func<T, TValue> elementSelector, IEqualityComparer<TKey> keyComparer = null)
        {
            return Dic<TKey, TValue>.FromDictionary(source.ToDictionary(keySelector, elementSelector, keyComparer));
        }

        /// <summary>
        /// イミュータブルな辞書を組み立てるための、書き込み用の辞書を生成する。<c>MoveToImmutable</c> で Dic に変換できる。
        /// </summary>
        public static DicBuilder<TKey, TValue> CreateBuilder<TKey, TValue>(int capacity = 0, IEqualityComparer<TKey> keyComparer = null)
        {
            return new DicBuilder<TKey, TValue>(new Dictionary<TKey, TValue>(capacity, keyComparer));
        }
    }
}
