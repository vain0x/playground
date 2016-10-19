using System.Collections.Generic;
using System.Linq;

namespace AsterSql.Core.SqlSyntax
{
    sealed class OrderKey
        : SqlPart
    {
        public ScalarSqlExpression Expression { get; }
        public OrderDirection Direction { get; }

        public OrderKey(ScalarSqlExpression expression, OrderDirection direction)
        {
            Expression = expression;
            Direction = direction;
        }

        #region Tokens
        IEnumerable<SqlToken> OrderKeywordTokens =>
            Direction == OrderDirection.Descending
            ? new[] { SqlToken.FromString("desc") }
            : Enumerable.Empty<SqlToken>();

        internal override IEnumerable<SqlToken> Tokens =>
            Expression.Tokens.Concat(OrderKeywordTokens);
        #endregion
    }
}
