using System;
using System.Data;
using FluentSqlBuilder.Detail;
using FluentSqlBuilder.Public;
using Xunit;

namespace FluentSqlBuilder.Test
{
    public class SqlExpressionTest
    {
        static SqlBuilder Sql => FakeDb.Sql;

        [Fact]
        public void TestCompoundExpression()
        {
            new CompoundExpression<IScalar<long>>(
                Sql,
                Sql.Int(1)
                .Concat(SqlPart.FromToken("+"))
                .Concat(Sql.Int(2))
            )
                .ToEmbeddedString()
                .ShouldEqual("1 + 2");
        }

        #region Test: As
        [Fact]
        public void TestAs_error()
        {
            Assert.Throws<ArgumentException>(() =>
                Sql.Null.As("invalid-alias").ToEmbeddedString()
            );
        }

        [Fact]
        public void TestAs_atomic()
        {
            Sql.Null.As("nil").ToEmbeddedString()
               .ShouldEqual("null as `nil`");
        }
        #endregion
    }
}
