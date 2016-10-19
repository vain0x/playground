using System;
using System.Data;
using System.Linq;
using Xunit;
using AsterSql.Core;
using AsterSql.SqlSyntax;

namespace AsterSql.Test
{
    public class SqlExpressionTest
    {
        static SqlBuilder Sql => FakeDb.Sql;

        [Fact]
        public void TestCompoundExpression()
        {
            new ConcreteScalarSqlExpression<long>(
                Sql,
                Sql.Int(1)
                    .Concat(SqlPart.FromString("+"))
                    .Concat(Sql.Int(2))
                    .Tokens
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

        [Fact]
        public void TestAs_relation()
        {
            FakeDb.Employee.Table.As("t").ToEmbeddedString()
                .ShouldEqual("`employees` as `t`");
        }

        [Fact]
        public void TestAs_idempotent()
        {
            Sql.Null<object>().As("nil").As("nil").ToEmbeddedString()
                .ShouldEqual("null as `nil`");

            FakeDb.Employee.Table.As("t").As("t").ToEmbeddedString()
                .ShouldEqual("`employees` as `t`");
        }

        [Fact]
        public void TestAs_aliased_expressions_cannot_be_aliased_as_other_name()
        {
            Assert.Throws<InvalidOperationException>(() =>
                Sql.Null<object>().As("nil").As("null").ToEmbeddedString()
            );

            Assert.Throws<InvalidOperationException>(() =>
                FakeDb.Employee.Table.As("e").As("m").ToEmbeddedString()
            );
        }
        #endregion
    }
}
