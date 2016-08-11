namespace FluentSqlBuilder.Detail
{
    public class OptionallyAliasedBuilder<TBase>
    {
        TBase _base;
        OptionallyAliasedExpression _aliased;

        public TBase As()
        {
            return _base;
        }

        public TBase As(string alias)
        {
            _aliased.AliasOrNull = alias;
            return _base;
        }

        public OptionallyAliasedBuilder(TBase @base, OptionallyAliasedExpression aliased)
        {
            _base = @base;
            _aliased = aliased;
        }
    }

    public static class OptionallyAliasedBuilder
    {
        public static OptionallyAliasedBuilder<B> Create<B>(B @base, OptionallyAliasedExpression aliased)
        {
            return new OptionallyAliasedBuilder<B>(@base, aliased);
        }
    }
}
