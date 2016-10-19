using Xunit;
using FluentSqlBuilder.SqlSyntax;

namespace FluentSqlBuilder.Test
{
    public class SqlConditionTest
    {
        static SqlBuilder Sql => FakeDb.Sql;

        [Fact]
        public void TestAnd()
        {
            Sql.Int(1).Equal(Sql.Int(2))
               .And(Sql.String("hello").IsNull())
               .ToEmbeddedString()
               .ShouldEqual("( 1 = 2 and 'hello' is null )");
        }

        [Fact]
        public void TestOr()
        {
            Sql.Int(1).IsNull()
               .Or(Sql.String("x").IsNull())
               .ToEmbeddedString()
               .ShouldEqual("( 1 is null or 'x' is null )");
        }
    }
}
