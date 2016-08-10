using System;

namespace FluentSqlBuilder.Detail
{
    public class FromlessSelectBuilder
        : InternalBuilder
    {
        readonly SelectStatement _statement;

        public FromlessSelectBuilder(SqlBuilder sqlBuilder, SelectStatement statement)
            : base(sqlBuilder)
        {
            _statement = statement;
        }

        #region From
        public OptionallyAliasedBuilder<FieldlessSelectBuilder> From(Expression relation)
        {
            var aliased = _statement.Source.Add(relation);
            var builder = new FieldlessSelectBuilder(SqlBuilder, _statement);
            return OptionallyAliasedBuilder.Create(builder, aliased);
        }

        public OptionallyAliasedBuilder<FieldlessSelectBuilder> From(string tableName)
        {
            return From(SqlBuilder.Table(tableName));
        }
        #endregion
    }
}
