using System;
using System.Data;
using FluentSqlBuilder.Detail;
using FluentSqlBuilder.Public;
using Xunit;

namespace FluentSqlBuilder.Test
{
    public class SqlExpressionTest
    {
        static SqlBuilder Sql => DummySqlBuilder.Sql;

        [Fact]
        public void TestCompoundExpression()
        {
            new CompoundExpression<IScalar<long>>(
                Sql,
                SqlPart.Concat(
                    Sql.Int(1),
                    SqlPart.FromToken("+"),
                    Sql.Int(2)
                ))
                .ToEmbeddedString()
                .ShouldEqual("1 + 2");
        }

        #region Test: As
        [Fact]
        public void TestAs_error()
        {
            Assert.Throws<ArgumentException>(() => Sql.Null.As("invalid-alias"));
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
