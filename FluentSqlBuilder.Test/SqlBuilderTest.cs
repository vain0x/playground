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
            var injectionalName = "'; delete from users 0 = 0 or '' = '";

            // Unqualified one.
            Assert.Equal("`person`", Sql.Table("person").ToString());
            Assert.ThrowsAny<Exception>(() => Sql.Table(injectionalName));

            // Qualified one.
            Assert.Equal("`db`.`person`", Sql.Table("db", "person").ToString());

            Assert.ThrowsAny<Exception>(() => Sql.Table(injectionalName, "person"));
        }

        [Fact]
        public void TableAsTest()
        {
            Assert.Equal("`person` as `p`", Sql.Table("person").As("p").ToString());

            Assert.ThrowsAny<Exception>(() => Sql.Table("person").As("p; delete from users"));
        }

        [Fact]
        public void ColumnTest()
        {
            Assert.Equal("`name`", Sql.Column("name").ToString());
            Assert.Equal("`p`.`name`", Sql.Column("p", "name").ToString());
            Assert.Equal("`name` as `n`", Sql.Column("name").As("n").ToString());
        }
        #endregion
    }
}
