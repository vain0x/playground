using System;

namespace FluentSqlBuilder.Detail
{
    public class FromlessSelectBuilder
        : InternalBuilder
    {
        SelectStatement Statement { get; }

        public FromlessSelectBuilder(SqlBuilder sqlBuilder, SelectStatement statement)
            : base(sqlBuilder)
        {
            Statement = statement;
        }

        #region From
        public FieldlessSelectBuilder From(SqlExpression relation)
        {
            Statement.Source.Add(relation);
            return new FieldlessSelectBuilder(SqlBuilder, Statement);
        }
        #endregion
    }
}
