using System;
using Optional;

namespace FluentSqlBuilder.SqlSyntax
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

        #region From
        public FieldlessSelectBuilder From(SqlExpression<IRelation> relation)
        {
            var statement = new SelectStatement(SqlBuilder, Combined, relation);
            return new FieldlessSelectBuilder(statement);
        }
        #endregion
    }
}
