using System;
using System.Linq;
using FluentSqlBuilder.Detail;
using FluentSqlBuilder.Public;
using Xunit;

namespace FluentSqlBuilder.Test
{
    public class SqlExpressionExtensionsTest
    {
        static SqlBuilder Sql => FakeDb.Sql;

        #region Test: Invoke
        [Fact]
        public void TestInvoke_no_arguments()
        {
            SqlExpressionExtensions
                .Invoke<IRelation>(Sql, "f", Enumerable.Empty<ISqlExpression<IScalar>>())
                .ToEmbeddedString()
                .ShouldEqual("f ( )");
        }

        [Fact]
        public void TestInvoke_two_arguments()
        {
            SqlExpressionExtensions
                .Invoke<IScalar<long>>(
                    Sql,
                    "max",
                    new[] { Sql.Int(1), Sql.Int(2) }
                )
                .ToEmbeddedString()
                .ShouldEqual("max ( 1 , 2 )");
        }
        #endregion

        #region Test: Normal operators
        [Fact]
        public void TestConcat()
        {
            Sql.String("hello")
               .Concat(Sql.String("world"))
               .ToEmbeddedString()
               .ShouldEqual("concat ( 'hello' , 'world' )");
        }
        #endregion

        #region Test: Condition operators
        [Fact]
        public void TestIsNull()
        {
            Sql.Null.IsNull()
               .ToEmbeddedString()
               .ShouldEqual("null is null");
        }

        [Fact]
        public void TestEqual()
        {
            Sql.Int(1).Equal(Sql.Int(2))
               .ToEmbeddedString()
               .ShouldEqual("1 = 2");
        }
        #endregion
    }
}
