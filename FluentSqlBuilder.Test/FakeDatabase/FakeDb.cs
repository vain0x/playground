using FluentSqlBuilder.Provider.Fake;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Test
{
    public static class FakeDb
    {
        public static FakeDbProvider Provider { get; } =
            new FakeDbProvider();

        public static SqlBuilder Sql { get; } =
            new SqlBuilder(Provider);
    }
}
