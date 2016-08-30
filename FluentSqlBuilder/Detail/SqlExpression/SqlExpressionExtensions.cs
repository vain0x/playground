using System;
using System.Collections.Generic;
using System.Linq;
using FluentSqlBuilder.Detail;

namespace FluentSqlBuilder.Public
{
    public static class SqlExpressionExtensions
    {
        public static ConditionBuilder
            Equal<X>(
                this ISqlExpression<IScalar<X>> lhs,
                ISqlExpression<IScalar<X>> rhs
            ) =>
            new ConditionBuilder(lhs.SqlBuilder).Equal(lhs, rhs);
    }
}
