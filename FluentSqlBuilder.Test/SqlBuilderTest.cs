using System;
using System.Data;
using System.Linq;
using Xunit;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Test
{
    public class SqlBuilderTest
    {
        static SqlBuilder Sql => FakeDb.Sql;

        #region Expression
        [Fact]
        public void TableTest()
        {
            var injectionalName = "'; delete from users 0 = 0 or '' = '";

            Assert.Equal("`person`", Sql.Table("person").ToString());
            Assert.ThrowsAny<Exception>(() => Sql.Table(injectionalName));
        }

        [Fact]
        public void TableAsTest()
        {
            Assert.Equal("`person` as `p`", Sql.Table("person").As("p").ToString());

            Assert.ThrowsAny<Exception>(() => Sql.Table("person").As("p; delete from users"));
        }
        #endregion
    }
}
