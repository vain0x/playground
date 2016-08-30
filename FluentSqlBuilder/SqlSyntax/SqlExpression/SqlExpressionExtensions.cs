using System;
using System.Collections.Generic;
using System.Linq;
using FluentSqlBuilder.Detail;

namespace FluentSqlBuilder.Public
{
    public static class SqlExpressionExtensions
    {
        public static ISqlCondition
            IsNull<X>(this ISqlExpression<IScalar<X>> lhs) =>
            new SqlCondition(lhs.SqlBuilder, SqlPart.FromToken("is null"));

        public static ISqlCondition
            Equal<X>(
                this ISqlExpression<IScalar<X>> lhs,
                ISqlExpression<IScalar<X>> rhs
            ) =>
            new SqlCondition(lhs.SqlBuilder, lhs, SqlPart.FromToken("="), rhs);
    }
}
