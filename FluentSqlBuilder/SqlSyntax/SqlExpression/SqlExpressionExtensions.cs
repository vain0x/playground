using System;
using System.Collections.Generic;
using System.Linq;
using FluentSqlBuilder.Detail;

namespace FluentSqlBuilder.Public
{
    public static class SqlExpressionExtensions
    {
        #region Internal
        public static ISqlExpression<IScalar<object>>
            Box<X>(this ISqlExpression<IScalar<X>> expression)
        {
            return new CompoundExpression<IScalar<object>>(
                expression.SqlBuilder,
                new SqlPart(expression.Tokens, expression.Parameters)
            );
        }

        static ISqlExpression<T>
            Invoke<T>(
                SqlBuilder sqlBuilder,
                string functionName,
                IEnumerable<ISqlExpression<IScalar<object>>> arguments
            ) 
            where T: ISqlTypeTag
            =>
            new CompoundExpression<T>(
                sqlBuilder,
                SqlPart.Concat(
                    new[] { SqlPart.FromToken(functionName) }
                    .Concat(
                        arguments
                        .Intersperse(SqlPart.FromToken(","))
                        .Enclose(SqlPart.FromToken("("), SqlPart.FromToken(")"))
                    )));
        #endregion

        #region Normal operators
        public static ISqlExpression<IScalar<string>>
            Concat(
                this ISqlExpression<IScalar<string>> lhs,
                params ISqlExpression<IScalar<string>>[] rhs
            ) =>
            Invoke<IScalar<string>>(lhs.SqlBuilder, "concat", new[] { lhs }.Concat(rhs));
        #endregion

        #region Condition operators
        public static ISqlCondition
            IsNull<X>(this ISqlExpression<IScalar<X>> lhs) =>
            new SqlCondition(lhs.SqlBuilder, lhs, SqlPart.FromToken("is null"));

        public static ISqlCondition
            Equal<X>(
                this ISqlExpression<IScalar<X>> lhs,
                ISqlExpression<IScalar<X>> rhs
            ) =>
            new SqlCondition(lhs.SqlBuilder, lhs, SqlPart.FromToken("="), rhs);
        #endregion
    }
}
