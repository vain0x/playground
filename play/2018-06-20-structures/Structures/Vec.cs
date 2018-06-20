using System;
using System.Collections.Generic;
using System.Linq;

namespace Structures
{
    /// <summary>
    /// Represents a vector (immutable array).
    ///
    /// <para>
    /// Very similar to immutable array but some differences:
    ///
    /// - is a class not struct,
    /// - structural equality and comparison,
    /// - simpler implementation by dropping keen perf. optimization,
    /// - shorter name.
    /// </para>
    /// </summary>
    public sealed class Vec<T>
        : IEquatable<Vec<T>>
        , IComparable
        , IComparable<Vec<T>>
        , IEnumerable<T>
        , IList<T>
    {
        T[] Buffer { get; }

        internal Vec(T[] buffer) => Buffer = buffer;

        internal static Vec<T> Empty { get; } =
            new Vec<T>(Array.Empty<T>());

        public int Length => Buffer.Length;

        public T this[int index] => Buffer[index];

        public override string ToString() =>
            "[" + string.Join(", ", this) + "]";

        #region Structural Equality

        public override bool Equals(object obj) =>
            Equals(obj as Vec<T>);

        public bool Equals(Vec<T> other) =>
            ReferenceEquals(this, other)
            || (!ReferenceEquals(other, null) && this.SequenceEqual(other));

        public override int GetHashCode() =>
            Buffer.Aggregate(19, (hash, item) =>
                unchecked(hash * 17 + EqualityComparer<T>.Default.GetHashCode(item))
            );

        public static bool operator ==(Vec<T> left, Vec<T> right) =>
            EqualityComparer<Vec<T>>.Default.Equals(left, right);

        public static bool operator !=(Vec<T> left, Vec<T> right) =>
            !(left == right);

        #endregion

        #region Structural Comparison

        public int CompareTo(Vec<T> other)
        {
            if (ReferenceEquals(other, null)) return 1;
            return CompareArrays(Buffer, other.Buffer);
        }

        /// <summary>
        /// Compare two arrays in lexicographical order.
        /// </summary>
        static int CompareArrays(T[] self, T[] other)
        {
            if (ReferenceEquals(self, other)) return 0;

            var len = Math.Min(self.Length, other.Length);
            for (var i = 0; i <  len; i++)
            {
                var c = Comparer<T>.Default.Compare(self[i], other[i]);
                if (c != 0) return c;
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
            ((IEnumerable<T>)Buffer).GetEnumerator();

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() =>
            GetEnumerator();

        int IList<T>.IndexOf(T item) =>
            ((IList<T>)Buffer).IndexOf(item);

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
            ((ICollection<T>)Buffer).Contains(item);

        void ICollection<T>.CopyTo(T[] array, int arrayIndex) =>
            ((ICollection<T>)Buffer).CopyTo(array, arrayIndex);

        int ICollection<T>.Count => Length;

        bool ICollection<T>.IsReadOnly => true;

        bool ICollection<T>.Remove(T item) =>
            throw new NotSupportedException();

        #endregion
    }

    public static class Vec
    {
        public static Vec<T> Empty<T>() =>
            Vec<T>.Empty;

        public static Vec<T> Of<T>(T item) =>
            new Vec<T>(new[] { item });

        public static Vec<T> Of<T>(T head, params T[] tail) =>
            new Vec<T>(new[] { head }.Concat(tail).ToArray());
    }

    public static class VecExtensions
    {
        public static Vec<T> ToVector<T>(this IEnumerable<T> source) =>
            new Vec<T>(source.ToArray());
    }
}
