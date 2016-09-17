using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;

namespace FluentSqlBuilder.SqlSyntax
{
    /// <summary>
    /// SQLの式を表す。
    /// </summary>
    /// <typeparam name="TType"></typeparam>
    public abstract class SqlExpression<TType>
        : SqlPart
        where TType: ISqlTypeTag
    {
        internal SqlBuilder SqlBuilder { get; }

        internal SqlExpression(SqlBuilder sqlBuilder)
        {
            SqlBuilder = sqlBuilder;
        }

        public AliasedSqlExpression<TType> As(string alias)
        {
            return new ConcreteAliasedSqlExpression<TType>(SqlBuilder, this, alias);
        }
    }
}
