using System;
using System.Data;
using System.Linq;
using Optional;
using Xunit;
using FluentSqlBuilder.SqlSyntax;

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

            Assert.Equal("`person`", Sql.Table(new object(), "person", Option.None<string>()).ToString());
            Assert.ThrowsAny<Exception>(() =>
                Sql.Table(new object(), injectionalName, "p".Some()).ToEmbeddedString()
            );
        }

        [Fact]
        public void TableAsTest()
        {
            Assert.Equal("`person` as `p`", Sql.Table(new object(), "person", "p".Some()).ToString());

            Assert.ThrowsAny<Exception>(() =>
                Sql.Table(new object(), "person", "p; delete from users".Some()).ToEmbeddedString()
            );
        }
        #endregion
    }
}
