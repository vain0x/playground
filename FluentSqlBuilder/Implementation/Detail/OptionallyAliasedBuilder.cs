using FluentSqlBuilder.Interface.Detail;

namespace FluentSqlBuilder.Implementation.Detail
{
    public class OptionallyAliased<TBaseInterface, TBaseImplementation>
        : TBaseImplementation
        , IOptionallyAliasedBuilder<TBaseInterface>
        where TBaseInterface: class
    {
        private readonly TBaseImplementation _base;

        public TBaseInterface As(string alias)
        {
            return _base as TBaseInterface;
        }
    }
}
