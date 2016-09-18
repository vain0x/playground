using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;

namespace FluentSqlBuilder.SqlSyntax
{
    /// <summary>
    /// SQLの式を表す。
    /// </summary>
    public abstract class SqlExpression
        : SqlPart
    {
        internal SqlBuilder SqlBuilder { get; }

        internal SqlExpression(SqlBuilder sqlBuilder)
        {
            SqlBuilder = sqlBuilder;
        }
    }

    public abstract class ScalarSqlExpression
        : SqlExpression
    {
        internal ScalarSqlExpression(SqlBuilder sqlBuilder)
            : base(sqlBuilder)
        {
        }
    }

    public abstract class ScalarSqlExpression<TValue>
        : ScalarSqlExpression
    {
        internal ScalarSqlExpression(SqlBuilder sqlBuilder)
            : base(sqlBuilder)
        {
        }

        public AliasedScalarSqlExpression<TValue> As(string alias)
        {
            return new AliasedScalarSqlExpression<TValue>(this, alias);
        }
    }

    public abstract class RelationSqlExpression
        : SqlExpression
    {
        internal RelationSqlExpression(SqlBuilder sqlBuilder)
            : base(sqlBuilder)
        {
        }

        public AliasedRelationSqlExpression As(string alias)
        {
            return new AliasedRelationSqlExpression(this, alias);
        }
    }
}
