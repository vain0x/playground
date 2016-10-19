using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AsterSql.Core.SqlSyntax
{
    public interface IAliasedSqlExpression
    {
        string Alias { get; }
    }

    public sealed class AliasedScalarSqlExpression<TValue>
        : ScalarSqlExpression<TValue>
        , IAliasedSqlExpression
    {
        ScalarSqlExpression<TValue> Expression { get; }
        public string Alias { get; }

        internal override IEnumerable<SqlToken> Tokens =>
            SqlBuilder.Language.ConstructAliasedExpression(Expression.Tokens, Alias);

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override AliasedScalarSqlExpression<TValue> As(string alias)
        {
            if (Alias != alias)
            {
                throw new InvalidOperationException("Aliased expression can't be aliased.");
            }
            return this;
        }

        internal AliasedScalarSqlExpression(ScalarSqlExpression<TValue> expression, string alias)
            : base(expression.SqlBuilder)
        {
            Expression = expression;
            Alias = alias;
        }
    }

    public sealed class AliasedRelationSqlExpression
        : RelationSqlExpression
        , IAliasedSqlExpression
    {
        RelationSqlExpression Expression { get; }
        public string Alias { get; }

        internal override IEnumerable<SqlToken> Tokens =>
            SqlBuilder.Language.ConstructAliasedExpression(Expression.Tokens, Alias);

        [EditorBrowsable(EditorBrowsableState.Never)]
        public override AliasedRelationSqlExpression As(string alias)
        {
            if (Alias != alias)
            {
                throw new InvalidOperationException("Aliased expression can't be aliased.");
            }
            return this;
        }

        internal AliasedRelationSqlExpression(RelationSqlExpression expression, string alias)
            : base(expression.SqlBuilder)
        {
            Expression = expression;
            Alias = alias;
        }
    }
}
