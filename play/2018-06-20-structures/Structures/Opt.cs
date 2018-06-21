using System;
using System.Collections.Generic;

namespace Structures
{
    /// <summary>
    /// Represents a value *maybe missing*.
    /// </summary>
    public struct Opt<T>
        : IEquatable<Opt<T>>
        , IComparable
        , IComparable<Opt<T>>
    {
        public bool IsSome { get; }

        T ValueOrDefault { get; }

        public T Value =>
            IsSome ? ValueOrDefault : throw new InvalidOperationException("No value.");

        internal Opt(T value)
        {
            if (value == null)
                throw new ArgumentNullException(nameof(value));

            IsSome = true;
            ValueOrDefault = value;
        }

        public T GetValueOrDefault(T defaultValue) =>
            IsSome ? Value : defaultValue;

        public bool Try(out T value)
        {
            value = ValueOrDefault;
            return IsSome;
        }

        public override string ToString() =>
            IsSome ? Value.ToString() : "";

        #region Structural Equality

        public override bool Equals(object obj) =>
            obj is Opt<T> && Equals((Opt<T>)obj);

        public bool Equals(Opt<T> other)
        {
            if (!IsSome)
                return !other.IsSome;

            return other.IsSome && EqualityComparer<T>.Default.Equals(Value, other.Value);
        }

        public override int GetHashCode() =>
            IsSome ? EqualityComparer<T>.Default.GetHashCode(Value) : 1;

        public static bool operator ==(Opt<T> left, Opt<T> right) =>
            left.Equals(right);

        public static bool operator !=(Opt<T> left, Opt<T> right) =>
            !(left == right);

        #endregion

        #region Structural Comparison

        public int CompareTo(Opt<T> other)
        {
            if (!IsSome)
                return other.IsSome ? 0 : 1;

            if (!other.IsSome)
                return -1;

            return Comparer<T>.Default.Compare(Value, other.Value);
        }

        int IComparable.CompareTo(object obj)
        {
            if (!(obj is Opt<T>))
                throw new ArgumentException("No comparison between different types.");

            return CompareTo((Opt<T>)obj);
        }

        public static bool operator <(Opt<T> left, Opt<T> right) =>
            Comparer<Opt<T>>.Default.Compare(left, right) < 0;

        public static bool operator >(Opt<T> left, Opt<T> right) =>
            right < left;

        public static bool operator <=(Opt<T> left, Opt<T> right) =>
            !(right < left);

        public static bool operator >=(Opt<T> left, Opt<T> right) =>
            !(left < right);

        #endregion

        public Opt<Y> Map<Y>(Func<T, Y> f) =>
            IsSome ? Opt.Some(f(Value)) : Opt.None<Y>();

        public Opt<Y> FlatMap<Y>(Func<T, Opt<Y>> f) =>
            IsSome ? f(Value) : Opt.None<Y>();
    }

    public static class Opt
    {
        public static Opt<T> None<T>() =>
            new Opt<T>();

        public static Opt<T> NotNull<T>(T value) =>
            value != null ? new Opt<T>(value) : new Opt<T>();

        public static Opt<T> Some<T>(T value) =>
            new Opt<T>(value);
    }

    public static class OptExtensions
    {
        public static Opt<T> RejectNull<T>(this T self) =>
            Opt.Some(self);

        public static Opt<T> AcceptNull<T>(this T self) =>
            Opt.NotNull(self);
    }
}
