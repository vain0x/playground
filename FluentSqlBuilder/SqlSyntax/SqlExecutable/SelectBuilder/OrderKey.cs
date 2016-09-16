using System.Collections.Generic;
using System.Data.Common;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public sealed class OrderKey
        : SqlPart
    {
        public SqlExpression<IScalar> Expression { get; }
        public OrderDirection Direction { get; }

        public OrderKey(SqlExpression<IScalar> expression, OrderDirection direction)
        {
            Expression = expression;
            Direction = direction;
        }

        #region SqlPart
        internal override IEnumerable<string> Tokens
        {
            get
            {
                foreach (var token in Expression.Tokens) yield return token;
                if (Direction == OrderDirection.Descending)
                {
                    yield return "desc";
                }
            }
        }

        internal override IEnumerable<DbParameter> Parameters =>
            Expression.Parameters;
        #endregion
    }
}
