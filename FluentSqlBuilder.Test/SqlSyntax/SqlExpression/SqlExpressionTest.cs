using System;
using System.Data;
using Xunit;
using FluentSqlBuilder.SqlSyntax;

namespace FluentSqlBuilder.Test
{
    public class SqlExpressionTest
    {
        static SqlBuilder Sql => FakeDb.Sql;

        [Fact]
        public void TestCompoundExpression()
        {
            new CompoundSqlExpression<IScalar<long>>(
                Sql,
                Sql.Int(1)
                .Concat(SqlPart.FromString("+"))
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
                Sql.Null<object>().As("invalid-alias").ToEmbeddedString()
            );
        }

        [Fact]
        public void TestAs_atomic()
        {
            Sql.Null<object>().As("nil").ToEmbeddedString()
               .ShouldEqual("null as `nil`");
        }
        #endregion

        #region Test: Quantification

        #endregion
    }
}
