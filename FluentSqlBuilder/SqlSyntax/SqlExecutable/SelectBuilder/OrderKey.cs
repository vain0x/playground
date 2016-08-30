using System.Collections.Generic;
using System.Data.Common;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class OrderKey
        : ISqlPart
    {
        public ISqlExpression<IScalar<object>> Expression { get; }
        public OrderDirection Direction { get; }

        public OrderKey(ISqlExpression<IScalar<object>> expression, OrderDirection direction)
        {
            Expression = expression;
            Direction = direction;
        }

        #region ISqlPart
        public IEnumerable<string> Tokens
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

        public IEnumerable<DbParameter> Parameters =>
            Expression.Parameters;
        #endregion
    }
}
