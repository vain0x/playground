using System;
using System.Collections.Generic;
using System.ComponentModel;

namespace Structures
{
    /// <summary>
    /// Represents a *maybe missing* value.
    ///
    /// This is a safe replacement of nullable values.
    ///
    /// <para>
    /// USAGE:
    ///
    /// <code>
    /// if (opt.IsSome) // Check the value existence first
    /// {
    ///     Use(opt.Value); // Use the actual value
    /// }
    /// </code>
    /// </para>
    ///
    /// <para>
    /// Unlike Option/Optional/Maybe/etc. from othe libraries,
    /// the inner value *NEVER* be null.
    /// </para>
    /// </summary>
    public struct Opt<T>
        : IEquatable<Opt<T>>
        , IComparable
        , IComparable<Opt<T>>
    {
        [EditorBrowsable(EditorBrowsableState.Never)]
        readonly T _value;

        /// <summary>
        /// Gets if the inner value exists.
        ///
        /// Returns <c>true</c> if existing; or <c>false</c> if missing.
        /// </summary>
        /// <value></value>
        public bool IsSome { get; }

        /// <summary>
        /// Gets if the inner value is missing.
        ///
        /// Returns <c>true</c> if missing; or <c>true</c> if existing.
        /// </summary>
        public bool IsNone =>
            !IsSome;

        /// <summary>
        /// Gets the inner value if existing. Check <c>IsSome</c> first for safety.
        /// </summary>
        /// <returns>
        /// Returns the inner value.
        /// </returns>
        /// <exception cref="InvalidOperationException">
        /// Thrown when the inner value is missing.
        /// </exception>
        public T Value =>
            IsSome ? _value : throw new InvalidOperationException("No value.");

        /// <summary>
        /// Constructs an existing value with the specified non-null value.
        /// </summary>
        /// <param name="value">
        /// A non-null value.
        /// </param>
        /// <exception cref="ArgumentNullException">
        /// Thrown when the specified value is null.
        /// </exception>
        public Opt(T value)
        {
            if (value == null)
                throw new ArgumentNullException(nameof(value));

            IsSome = true;
            _value = value;
        }

        /// <summary>
        /// Gets the inner value, or a default value.
        /// </summary>
        /// <param name="defaultValue">
        /// The value to be returned if the inner value is missing.
        /// </param>
        /// <returns>
        /// Returns the inner value if exists, or the specified value.
        /// </returns>
        public T GetValueOrDefault(T defaultValue) =>
            IsSome ? Value : defaultValue;

        /// <summary>
        /// Tries to retrieve the inner value.
        /// </summary>
        /// <param name="value">
        /// The variable to be set to the inner value.
        /// </param>
        /// <returns>
        /// Returns <c>true</c> if success.
        /// </returns>
        public bool Try(out T value)
        {
            value = _value;
            return IsSome;
        }

        /// <summary>
        /// Gets the string that represents the inner value, or an empty string.
        /// </summary>
        /// <returns>
        /// Returns <c>innerValue.ToString()</c> or <c>""</c>.
        /// </returns>
        public override string ToString() =>
            IsSome ? Value.ToString() : "";

        #region Structural Equality

        /// <summary>
        /// Gets if the two values are equal.
        /// </summary>
        /// <param name="obj">
        /// The compared value.
        /// </param>
        /// <returns>
        /// Returns <c>true</c> if
        ///
        /// the two values have the same type
        /// and both existing or missing
        /// and inner values are equal with the default comparer.
        /// </returns>
        public override bool Equals(object obj) =>
            obj is Opt<T> && Equals((Opt<T>)obj);

        /// <summary>
        /// Gets if the two values are equal.
        /// </summary>
        /// <param name="other">
        /// The compared value.
        /// </param>
        /// <returns>
        /// returns <c>true</c> if
        ///
        /// The two values are both existing or missing
        /// and inner values are equal with the default comparer.
        /// </returns>
        public bool Equals(Opt<T> other)
        {
            if (!IsSome)
                return !other.IsSome;

            return other.IsSome && EqualityComparer<T>.Default.Equals(Value, other.Value);
        }

        /// <summary>
        /// Retrieves the hash code based on the inner value's hash code.
        /// </summary>
        /// <returns></returns>
        public override int GetHashCode()
        {
            if (!IsSome)
                return 1;

            return EqualityComparer<T>.Default.GetHashCode(Value);
        }

        /// <summary>
        /// Gets if the two values are equal. See <c>Equals</c> for details.
        /// </summary>
        public static bool operator ==(Opt<T> left, Opt<T> right) =>
            left.Equals(right);

        /// <summary>
        /// Gets if the two values aren't equal. See <c>Equals</c> for details.
        /// </summary>
        public static bool operator !=(Opt<T> left, Opt<T> right) =>
            !(left == right);

        #endregion

        #region Structural Comparison

        /// <summary>
        /// Compare the two values.
        ///
        /// <c>None</c>s are lesser than <c>Some</c>s.
        /// </summary>
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

        // NOTE: Don't provide map/flat map because
        // this is NOT intended to encourage monadic programming
        // but basic functional programming.

        // public Opt<Y> Map<Y>(Func<T, Y> f) =>
        //     IsSome ? Opt.Some(f(Value)) : Opt.None<Y>();

        // public Opt<Y> FlatMap<Y>(Func<T, Opt<Y>> f) =>
        //     IsSome ? f(Value) : Opt.None<Y>();
    }

    /// <summary>
    /// Provides static methods to create optional values.
    /// </summary>
    public static class Opt
    {
        /// <summary>
        /// Gets a *missing* value, similar to <c>null</c>.
        /// </summary>
        /// <typeparam name="T">
        /// The type of inner value.
        /// </typeparam>
        /// <returns>
        /// Returns <c>None</c>.
        /// </returns>
        public static Opt<T> None<T>() =>
            new Opt<T>();

        /// <summary>
        /// Wraps a non-null value with <c>Some</c>.
        /// </summary>
        /// <param name="value">
        /// A value maybe null.
        /// </param>
        /// <typeparam name="T">
        /// The type of value.
        /// </typeparam>
        /// <returns>
        /// Returns the optional.
        /// </returns>
        /// <exception cref="InvalidOperationException">
        /// Thrown when the specified value is null.
        /// </exception>
        public static Opt<T> Some<T>(T value) =>
            new Opt<T>(value);

        /// <summary>
        /// Checks if the value is null
        /// and converts to optional safely.
        /// </summary>
        /// <param name="value">
        /// A value, maybe null.
        /// </param>
        /// <typeparam name="T">
        /// The type of value.
        /// </typeparam>
        /// <returns>
        /// Returns <c>Some(value)</c> if value is not null
        /// or <c>None</c> if the value is null.
        /// </returns>
        public static Opt<T> AllowNull<T>(T value) =>
            value != null ? new Opt<T>(value) : new Opt<T>();

        /// <summary>
        /// Converts a nullable struct to optional.
        /// </summary>
        public static Opt<T> AllowNull<T>(T? value) where T : struct =>
            value.HasValue ? new Opt<T>(value.Value) : new Opt<T>();
    }
}
