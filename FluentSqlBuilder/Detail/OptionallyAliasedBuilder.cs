using System;

namespace FluentSqlBuilder.Detail
{
    public abstract class OptionallyAliasedBuilder<TBase>
    {
        public abstract TBase As(string alias);
    }

    public class OptionallyAliasedBuilder<TBase, TValue>
        : OptionallyAliasedBuilder<TBase>
    {
        private TBase _base;
        private OptionallyAliased<TValue> _aliased;

        public override TBase As(string alias)
        {
            if (alias == null) throw new ArgumentNullException("alias");
            _aliased.AliasOrNull = alias;
            return _base;
        }

        public OptionallyAliasedBuilder(TBase @base, OptionallyAliased<TValue> aliased)
        {
            _base = @base;
            _aliased = aliased;
        }
    }

    public static class OptionallyAliasedBuilder
    {
        public static OptionallyAliasedBuilder<B> Create<B, X>(B @base, OptionallyAliased<X> aliased)
        {
            return new OptionallyAliasedBuilder<B, X>(@base, aliased);
        }
    }
}
