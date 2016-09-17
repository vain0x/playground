using System;
using System.Collections.Generic;
using System.Linq;

namespace FluentSqlBuilder.SqlSyntax
{
    public static class SqlExpressionExtensions
    {
        #region Internal
        internal static SqlPart Invoke(
            string functionName,
            IEnumerable<ScalarSqlExpression> arguments
        )
        {
            return
                SqlPart.Concat(
                    new[] { SqlPart.FromString(functionName) }
                    .Concat(
                        arguments
                        .Intersperse(SqlPart.FromString(","))
                        .Enclose(SqlPart.FromString("("), SqlPart.FromString(")"))
                    ));
        }

        #endregion

        #region Cast operators
        public static ScalarSqlExpression<X> Unbox<X>(this ScalarSqlExpression expression)
        {
            return new ConcreteScalarSqlExpression<X>(expression.SqlBuilder, expression.Tokens);
        }
        #endregion

        #region Normal operators
        public static ScalarSqlExpression<string> Concat(
            this ScalarSqlExpression<string> lhs,
            params ScalarSqlExpression<string>[] rhs
        )
        {
            var part = Invoke("concat", new[] { lhs }.Concat(rhs));
            return new ConcreteScalarSqlExpression<string>(lhs.SqlBuilder, part);
        }
        #endregion

        #region Condition operators
        public static SqlCondition IsNull<X>(
            this ScalarSqlExpression<X> lhs
        ) =>
            new AtomicSqlCondition(
                lhs.SqlBuilder,
                lhs.Concat(SqlPart.FromString("is null"))
            );

        public static SqlCondition Equal<X>(
            this ScalarSqlExpression<X> lhs,
            ScalarSqlExpression<X> rhs
        ) =>
            new AtomicSqlCondition(
                lhs.SqlBuilder,
                lhs.Concat(SqlPart.FromString("=")).Concat(rhs)
            );
        #endregion
    }
}
