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

        public OptionallyAliasedBuilder<FieldlessSelectBuilder> From(string tableName)
        {
            var aliased = new OptionallyAliased<Expression>(SqlBuilder.Table(tableName));
            _statement.Source.Relations.Add(aliased);
            var builder = new FieldlessSelectBuilder(SqlBuilder, _statement);
            return OptionallyAliasedBuilder.Create(builder, aliased);
        }

        public OptionallyAliasedBuilder<FieldlessSelectBuilder> From(string qualifier, string tableName)
        {
            throw new NotImplementedException();
        }
    }
}
