using Xunit;
using FluentSqlBuilder.Provider.Fake;

namespace FluentSqlBuilder.Test
{
    public class SelectBuilderTest
    {
        SqlBuilder Sql { get; } =
            new SqlBuilder(new FakeDbProvider());

        [Fact]
        public void MinimumTest()
        {
            var c = Sql.Select()
                .From(Sql.Table("persons"))
                .Field(Sql.Column<string>("name"))
                .ToCommand();
            Assert.Equal("select `name` from `persons`", c.CommandText);
            Assert.True(c.Parameters.Count == 0);
        }

        [Fact]
        public void WhereTest()
        {
            var c = Sql.Select()
                .From(Sql.Table("persons"))
                .Where().Equal(Sql.Column<string>("name"), Sql.String("Miku"))
                .Field(Sql.Column<long>("age"))
                .ToCommand();
            Assert.Equal(
                "select `age` from `persons` where ( `name` = @p0 )",
                c.ToParameterizedString()
            );
        }
    }
}
