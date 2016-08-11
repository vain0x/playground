using System;
using System.Data;
using System.Linq;
using Xunit;
using FluentSqlBuilder.Detail;
using FluentSqlBuilder.Provider.Fake;

namespace FluentSqlBuilder.Test
{
    public class SqlBuilderTest
    {
        static FakeDbProvider Provider { get; } =
            new FakeDbProvider();

        static SqlBuilder Sql { get; } =
            new SqlBuilder(Provider);

        #region Expression
        [Fact]
        public void TableTest()
        {
            // Unqualified one.
            Assert.Equal("`person`", Sql.Table("person").ToString());
            Assert.ThrowsAny<Exception>(
                () => Sql.Table("person '; DELETE FROM users")
            );

            // Qualified one.
            Assert.Equal("`db`.`person`", Sql.Table("db", "person").ToString());
        }

        [Fact]
        public void TableAsTest()
        {
            // TODO: Should validate and quote aliases.
            Assert.Equal("`person` as p", Sql.Table("person").As("p").ToString());
        }

        [Fact]
        public void ColumnTest()
        {
            Assert.Equal("`name`", Sql.Column("name").ToString());
            Assert.Equal("`p`.`name`", Sql.Column("p", "name").ToString());
        }
        #endregion
    }
}
