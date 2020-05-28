using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Structures
{
    /// <summary>
    /// Represents a vector (immutable array).
    ///
    /// <para>
    /// Vec is the most suitable collection for much situations.
    /// The usage is the same as arrays/lists
    /// except that modifications such as <c>Add</c>, <c>v[i] = x</c> etc. are disallowed.
    /// </para>
    ///
    /// <para>
    /// Very similar to <c>ImmutableArray</c> from <c>System.Collections.Immutable</c>
    /// but some differences:
    ///
    /// - is a class rather than struct,
    /// - structural equality and comparison for usability,
    /// - simpler implementation for understandability,
    /// - shorter name for familiarity.
    /// </para>
    /// </summary>
    public sealed class Vec<T>
        : IEquatable<Vec<T>>
        , IComparable
        , IComparable<Vec<T>>
        , IEnumerable<T>
        , IList<T>
    {
        readonly T[] _buffer;

        // NOTE: Guide for users who don't read docs.
        [Obsolete("Use Vec.Empty<T>() instead")]
        public Vec() => _buffer = Array.Empty<T>();

        [Obsolete("Use array.ToVec() instead")]
        public Vec(T[] array) => _buffer = array.ToArray();

        // HACK: The first argument is just for overloading.
        internal Vec(object _tag, T[] buffer)
        {
            Debug.Assert(buffer != null);

            _buffer = buffer;
        }

        internal static readonly Vec<T> Empty =
            new Vec<T>(null, Array.Empty<T>());

        public int Length => _buffer.Length;

        public T this[int index] => _buffer[index];

        public override string ToString() =>
            "[" + string.Join(", ", this) + "]";

        #region Structural Equality

        public override bool Equals(object obj) =>
            Equals(obj as Vec<T>);

        public bool Equals(Vec<T> other)
        {
            if (ReferenceEquals(other, null))
                return false;
            if (ReferenceEquals(this, other))
                return true;

            return this.SequenceEqual(other);
        }

        public override int GetHashCode()
        {
            var h = 19;
            for (var i = 0; i < _buffer.Length; i++)
            {
                h = unchecked(h * 17 + EqualityComparer<T>.Default.GetHashCode(_buffer[i]));
            }
            return h;
        }

        public static bool operator ==(Vec<T> left, Vec<T> right) =>
            EqualityComparer<Vec<T>>.Default.Equals(left, right);

        public static bool operator !=(Vec<T> left, Vec<T> right) =>
            !(left == right);

        #endregion

        #region Structural Comparison

        public int CompareTo(Vec<T> other)
        {
            if (ReferenceEquals(other, null))
                return 1;

            return CompareArrays(_buffer, other._buffer);
        }

        /// <summary>
        /// Compare two arrays in lexicographical order.
        /// </summary>
        static int CompareArrays(T[] self, T[] other)
        {
            if (ReferenceEquals(self, other))
                return 0;

            var len = Math.Min(self.Length, other.Length);
            for (var i = 0; i < len; i++)
            {
                var c = Comparer<T>.Default.Compare(self[i], other[i]);
                if (c != 0)
                    return c;
            }

            return self.Length.CompareTo(other.Length);
        }

        int IComparable.CompareTo(object obj)
        {
            if (obj != null && !(obj is Vec<T>))
                throw new ArgumentException("No comparison between different type.");

            return CompareTo(obj as Vec<T>);
        }

        public static bool operator <(Vec<T> left, Vec<T> right) =>
            Comparer<Vec<T>>.Default.Compare(left, right) < 0;

        public static bool operator >(Vec<T> left, Vec<T> right) =>
            right < left;

        public static bool operator <=(Vec<T> left, Vec<T> right) =>
            !(right < left);

        public static bool operator >=(Vec<T> left, Vec<T> right) =>
            !(left < right);

        #endregion

        #region IList[T]

        public IEnumerator<T> GetEnumerator() =>
            ((IEnumerable<T>)_buffer).GetEnumerator();

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() =>
            GetEnumerator();

        int IList<T>.IndexOf(T item) =>
            ((IList<T>)_buffer).IndexOf(item);

        void IList<T>.Insert(int index, T item) =>
            throw new NotSupportedException();

        void IList<T>.RemoveAt(int index) =>
            throw new NotSupportedException();

        T IList<T>.this[int index]
        {
            get => this[index];
            set => throw new NotSupportedException();
        }

        void ICollection<T>.Add(T item) =>
            throw new NotSupportedException();

        void ICollection<T>.Clear() =>
            throw new NotSupportedException();

        bool ICollection<T>.Contains(T item) =>
            ((ICollection<T>)_buffer).Contains(item);

        void ICollection<T>.CopyTo(T[] array, int arrayIndex) =>
            ((ICollection<T>)_buffer).CopyTo(array, arrayIndex);

        int ICollection<T>.Count => Length;

        bool ICollection<T>.IsReadOnly => true;

        bool ICollection<T>.Remove(T item) =>
            throw new NotSupportedException();

        #endregion
    }

    public static class Vec
    {
        /// <summary>
        /// Gets an empty vector.
        /// </summary>
        /// <typeparam name="T">
        /// The type of items.
        /// </typeparam>
        /// <returns>
        /// Returns a vector with length 0.
        /// </returns>
        public static Vec<T> Empty<T>() =>
            Vec<T>.Empty;

        /// <summary>
        /// Creates an 1-length vector with the specified item.
        /// </summary>
        /// <param name="item">
        /// A value of item.
        /// </param>
        /// <typeparam name="T">
        /// The type of values.
        /// </typeparam>
        /// <returns>
        /// Returns a vector with length 1 that contains the specified value.
        /// </returns>
        public static Vec<T> Of<T>(T item) =>
            new Vec<T>(null, new[] { item });

        /// <summary>
        /// Creates a vector with the specified items.
        /// This is alternative of the <c>new[] { .. }</c> notation for arrays.
        ///
        /// <para>
        /// EXAMPLE: <c>Vec.Of(1, 2, 3)</c>
        /// </para>
        /// </summary>
        /// <param name="head">
        /// The first item.
        /// </param>
        /// <param name="tail">
        /// The second and more items.
        /// </param>
        /// <typeparam name="T">
        /// The type of items.
        /// </typeparam>
        /// <returns>
        /// Returns a vector with length equal to the number of arguments
        /// that contains the specified arguments in the same order.
        /// </returns>
        public static Vec<T> Of<T>(T head, params T[] tail) =>
            new Vec<T>(null, new[] { head }.Concat(tail).ToArray());
    }

    /// <summary>
    /// Provides extension methods for vectors.
    /// </summary>
    public static class VecExtensions
    {
        /// <summary>
        /// Creates a vector from the sequence.
        /// </summary>
        /// <param name="source">
        /// A sequence of items.
        /// </param>
        /// <typeparam name="T">
        /// The type of the items.
        /// </typeparam>
        /// <returns>
        /// Returns a vector with length equals to the number of elements in the sequence
        /// that contains elements yielded by the sequence in the same order.
        /// </returns>
        /// <exception cref="ArgumentNullException">
        /// Thrown when the source is null.
        /// </exception>
        public static Vec<T> ToVec<T>(this IEnumerable<T> source) =>
            new Vec<T>(null, source.ToArray());
    }
}
