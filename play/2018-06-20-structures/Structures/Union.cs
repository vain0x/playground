using System;
using System.Collections.Generic;
using System.Linq;

namespace Structures
{
    public abstract class UnionBase<TUnion>
    {
        public sealed class Variant<T>
            : IVariant<TUnion, T>
        {
            public static implicit operator Variant<T>(VariantSeed _) => new Variant<T>();
        }

        protected static VariantSeed V { get; } = new VariantSeed();

        public static Matcher<TUnion, Sum<T>> Matcher<T>(Variant<T> variant)
        {
            throw new NotImplementedException();
        }
    }

    public sealed class Matcher<TUnion, TSum>
    {
        public Matcher<TUnion, Sum<T, TSum>> Or<T>(UnionBase<TUnion>.Variant<T> variant)
        {
            throw new NotImplementedException();
        }
    }

    public sealed class Matcher<TUnion, TSum, TResult>
    {
    }

    public static class MatcherExtensions
    {
        public static R Run<TUnion, R>(this Matcher<TUnion, R> self)
        {
            throw new InvalidOperationException();
        }

        public static R Match<TUnion, T, R>(
            this Matcher<TUnion, Sum<T>, R> matcher,
            UnionBase<TUnion>.Variant<T> variant,
            Func<T, R> f
        )
        {
            throw new InvalidOperationException();
        }

        public static Matcher<TUnion, TSum, R> Match<TUnion, TSum, T, R>(
            this Matcher<TUnion, Sum<T, TSum>, R> matcher,
            UnionBase<TUnion>.Variant<T> variant,
            Func<T, R> f
        )
        {
            throw new InvalidOperationException();
        }

        public static Matcher<TUnion, TSum, R> Match<TUnion, TSum, T, R>(
            this Matcher<TUnion, Sum<T, TSum>> matcher,
            UnionBase<TUnion>.Variant<T> variant,
            Func<T, R> f
        )
        {
            throw new InvalidOperationException();
        }
    }

    public interface ISum
    {
    }

    public struct Sum
        : ISum
    {
    }

    public struct Sum<T>
        : ISum
    {
    }

    public struct Sum<L, R>
        : ISum
    {
    }

    public sealed class VariantSeed
    {
    }

    public interface IVariant<TUnion>
    {
    }

    public interface IVariant<TUnion, T>
    {
    }

    public sealed class JsonValue
        : UnionBase<JsonValue>
    {
        public static Variant<decimal> Number = V;
        public static Variant<string> String = V;

        public static Matcher<JsonValue, Sum<decimal, Sum<string>>> Matcher { get; } =
            Matcher(String).Or(Number);
    }

    public class Sample
    {
        public void F()
        {
            var j = default(JsonValue);
            var typeId =
                JsonValue.Matcher
                .Match(JsonValue.Number, n => 0)
                .Match(JsonValue.String, s => 1);
        }
    }
}
