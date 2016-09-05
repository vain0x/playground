using System;
using Optional;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class FromlessSelectBuilder
    {
        SqlBuilder SqlBuilder { get; }
        Option<CombinedSelectStatement> Combined { get; }

        public FromlessSelectBuilder(SqlBuilder sqlBuilder, Option<CombinedSelectStatement> combined)
        {
            SqlBuilder = sqlBuilder;
            Combined = combined;
        }

        #region From
        public FieldlessSelectBuilder From(ISqlExpression<IRelation> relation)
        {
            var statement = new SelectStatement(SqlBuilder, Combined);
            statement.Source.Add(relation);
            return new FieldlessSelectBuilder(statement);
        }
        #endregion
    }
}
