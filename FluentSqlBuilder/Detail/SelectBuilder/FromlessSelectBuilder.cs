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
        public OptionallyAliasedBuilder<FieldlessSelectBuilder> From(Expression relation)
        {
            var aliased = Statement.Source.Add(relation);
            var builder = new FieldlessSelectBuilder(SqlBuilder, Statement);
            return OptionallyAliasedBuilder.Create(builder, aliased);
        }

        public OptionallyAliasedBuilder<FieldlessSelectBuilder> From(string tableName)
        {
            return From(SqlBuilder.Table(tableName));
        }
        #endregion
    }
}
