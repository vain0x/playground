using System;
using System.Data;
using System.Linq;
using Xunit;
using FluentSqlBuilder.Detail;
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

            Assert.Equal("`person`", Sql.Table("person", "person").ToString());
            Assert.ThrowsAny<Exception>(() =>
                Sql.Table(injectionalName, "p").ToEmbeddedString()
            );
        }

        [Fact]
        public void TableAsTest()
        {
            Assert.Equal("`person` as `p`", Sql.Table("person", "p").ToString());

            Assert.ThrowsAny<Exception>(() =>
                Sql.Table("person", "p; delete from users").ToEmbeddedString()
            );
        }
        #endregion
    }
}
