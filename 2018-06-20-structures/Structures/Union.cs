using System;
using System.Collections.Generic;
using System.Collections.Specialized;

namespace Structures
{
    public struct Unit
    { }

    public interface ITys
    { }

    public sealed class Tys<THead, TTail>
        : ITys
    { }

    public sealed class Tys
        : ITys
    { }

    public sealed class Matcher<TUnion, TVariantTypes>
    {
        public static readonly Matcher<TUnion, TVariantTypes> Instance = new Matcher<TUnion, TVariantTypes>();

        public Matcher<TUnion, Tys<TVariant, TVariantTypes>> Or<TVariant>()
        {
            return Matcher<TUnion, Tys<TVariant, TVariantTypes>>.Instance;
        }

        public static implicit operator Matcher<TUnion, TVariantTypes>(ImplicitMatcher _)
        {
            return Instance;
        }
    }

    public sealed class MatcherC<TUnion, TRestTypes, TResult>
    {
        internal Func<TUnion, TResult> Match;
    }

    public static class MatcherExtensions
    {
        // first with
        public static MatcherC<TUnion, TRestTypes, TResult> With<TUnion, TVariant, TRestTypes, TResult>(
            this Matcher<TUnion, Tys<TVariant, TRestTypes>> matcher,
            Func<TVariant, TResult> arm
        )
            where TUnion : UnionBase<TUnion>, new()
            where TVariant : IVariant<TUnion, TVariant>, new()
        {
            return new MatcherC<TUnion, TRestTypes, TResult>()
            {
                Match = u => arm(u.As<TVariant>()),
            };
        }

        // middle with
        public static MatcherC<TUnion, TRestTypes, TResult> With<TUnion, TVariant, TRestTypes, TResult>(
            this MatcherC<TUnion, Tys<TVariant, TRestTypes>, TResult> matcher,
            Func<TVariant, TResult> arm
        )
            where TUnion : UnionBase<TUnion>, new()
            where TVariant : IVariant<TUnion, TVariant>, new()
        {
            return new MatcherC<TUnion, TRestTypes, TResult>()
            {
                Match = u =>
                {
                    TVariant v;
                    if (!u.TryCast(out v)) return matcher.Match(u);
                    return arm(v);
                },
            };
        }

        // final with
        public static Func<TUnion, TResult> With<TUnion, TVariant, TResult>(
            this MatcherC<TUnion, Tys<TVariant, Tys>, TResult> matcher,
            Func<TVariant, TResult> arm
        )
            where TUnion : UnionBase<TUnion>, new()
            where TVariant : IVariant<TUnion, TVariant>, new()
        {
            return u =>
            {
                TVariant v;
                if (!u.TryCast(out v)) return matcher.Match(u);
                return arm(v);
            };
        }
    }

    public interface IVariant<TUnion>
        where TUnion : UnionBase<TUnion>, new()
    {
        TVariant As<TVariant>()
            where TVariant : IVariant<TUnion, TVariant>, new();
    }

    public interface IVariant<TUnion, TVariant>
        : IVariant<TUnion>
        where TUnion : UnionBase<TUnion>, new()
        where TVariant : IVariant<TUnion, TVariant>, new()
    {
    }

    public abstract class VariantBase<TUnion, TVariant, T>
        : IVariant<TUnion, TVariant>
        where TUnion : UnionBase<TUnion>, new()
        where TVariant : VariantBase<TUnion, TVariant, T>, new()
    {
        T FieldValue;

        public T Value
        {
            get { return FieldValue; }
        }

        public static TUnion Create(T value)
        {
            var v = new TVariant();
            v.FieldValue = value;
            var u = new TUnion();
            u.V = v;
            return u;
        }

        public V As<V>()
            where V : IVariant<TUnion, V>, new()
        {
            throw new NotImplementedException();
        }
    }

    public abstract class UnionBase<TUnion>
        : IVariant<TUnion>
        where TUnion : UnionBase<TUnion>, new()
    {
        public abstract class Variant<TVariant, T>
            : VariantBase<TUnion, TVariant, T>
            where TVariant : VariantBase<TUnion, TVariant, T>, new()
        {
        }

        internal IVariant<TUnion> V;

        public TVariant As<TVariant>()
            where TVariant : IVariant<TUnion, TVariant>, new()
        {
            return V is TVariant ? (TVariant)V : default(TVariant);
        }

        public bool TryCast<TVariant>(out TVariant v)
        {
            if (V is TVariant)
            {
                v = (TVariant)V;
                return true;
            }

            v = default(TVariant);
            return false;
        }

        protected static Matcher<TUnion, Tys<TVariant, Tys>> Matcher<TVariant>()
        {
            return Matcher<TUnion, Tys<TVariant, Tys>>.Instance;
        }

        protected static ImplicitMatcher ImplicitMatcher
        {
            get { return new ImplicitMatcher(); }
        }
    }

    public struct ImplicitMatcher { }
}
