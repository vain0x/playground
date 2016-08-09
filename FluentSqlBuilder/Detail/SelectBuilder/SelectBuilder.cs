using System;

namespace FluentSqlBuilder.Detail
{
    public class SelectBuilder
    {
        readonly SelectStatement _statement;

        public SelectBuilder(SelectStatement statement)
        {
            _statement = statement;
        }

        #region Field
        public OptionallyAliasedBuilder<SelectBuilder> Field(Expression expression)
        {
            var field = new OptionallyAliased<Expression>(expression);
            _statement.Fields.Add(field);
            return OptionallyAliasedBuilder.Create(this, field);
        }
        #endregion

        #region Union
        public FromlessSelectBuilder Union()
        {
            throw new NotImplementedException();
        }
        #endregion
    }
}
