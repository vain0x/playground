using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FluentSqlBuilder.SqlSyntax
{
    public abstract class AliasedSqlExpression<TType>
        : SqlExpression<TType>
        where TType : ISqlTypeTag
    {
        public abstract string Alias { get; }

        protected AliasedSqlExpression(SqlBuilder sqlBuilder)
            : base(sqlBuilder)
        {
        }
    }

    sealed class ConcreteAliasedSqlExpression<TType>
        : AliasedSqlExpression<TType>
        where TType : ISqlTypeTag
    {
        public SqlExpression<TType> Expression { get; }
        public sealed override string Alias { get; }

        public ConcreteAliasedSqlExpression(
            SqlBuilder sqlBuilder,
            SqlExpression<TType> expression,
            string alias
        )
            : base(sqlBuilder)
        {
            Expression = expression;
            Alias = alias;
        }

        internal override IEnumerable<SqlToken> Tokens =>
            Expression.Tokens
            .Concat(
                new[]
                {
                    SqlToken.FromString("as"),
                    SqlToken.FromString(SqlBuilder.Language.QuoteIdentifier(Alias))
                });
    }
}
