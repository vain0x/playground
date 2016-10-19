using System;
using System.Collections.Generic;
using System.Linq;

namespace AsterSql.SqlSyntax
{
    public static class SqlExpressionExtensions
    {
        #region Internal
        internal static IEnumerable<SqlToken> Invoke(
            string functionName,
            IEnumerable<ScalarSqlExpression> arguments
        )
        {
            var argumentList =
                arguments
                .Select(a => a.Tokens)
                .Intercalate(new[] { SqlToken.FromString(",") })
                .Enclose("(", ")");
            return
                new[] { SqlToken.FromString(functionName) }.Concat(argumentList);
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
        )
        {
            return
                new AtomicSqlCondition(
                    lhs.SqlBuilder,
                    lhs.Concat(SqlPart.FromString("is null")).Tokens
                );
        }

        public static SqlCondition Equal<X>(
            this ScalarSqlExpression<X> lhs,
            ScalarSqlExpression<X> rhs
        )
        {
            return
                new AtomicSqlCondition(
                    lhs.SqlBuilder,
                    lhs.Concat(SqlPart.FromString("=")).Concat(rhs).Tokens
                );
        }
        #endregion
    }
}
