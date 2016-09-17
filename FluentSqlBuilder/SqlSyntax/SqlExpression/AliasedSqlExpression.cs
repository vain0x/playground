using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FluentSqlBuilder.SqlSyntax
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
            Expression.Tokens
            .Concat(
                new[]
                {
                    SqlToken.FromString("as"),
                    SqlToken.FromString(SqlBuilder.Language.QuoteIdentifier(Alias))
                });

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
            Expression.Tokens
            .Concat(
                new[]
                {
                    SqlToken.FromString("as"),
                    SqlToken.FromString(SqlBuilder.Language.QuoteIdentifier(Alias))
                });

        internal AliasedRelationSqlExpression(RelationSqlExpression expression, string alias)
            : base(expression.SqlBuilder)
        {
            Expression = expression;
            Alias = alias;
        }
    }
}
