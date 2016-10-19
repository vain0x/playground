using System;
using Optional;

namespace AsterSql.Core.SqlSyntax
{
    public sealed class FromlessSelectBuilder
    {
        SqlBuilder SqlBuilder { get; }
        Option<CombinedSelectStatement> Combined { get; }

        internal FromlessSelectBuilder(SqlBuilder sqlBuilder, Option<CombinedSelectStatement> combined)
        {
            SqlBuilder = sqlBuilder;
            Combined = combined;
        }

        public FieldlessSelectBuilder From(RelationSqlExpression relation)
        {
            var statement = new SelectStatement(SqlBuilder, Combined, relation);
            return new FieldlessSelectBuilder(statement);
        }
    }
}
