using System;

namespace FluentSqlBuilder.Detail
{
    public class SelectBuilder
    {
        SelectStatement Statement { get; }

        public SelectBuilder(SelectStatement statement)
        {
            Statement = statement;
        }

        #region Field
        public OptionallyAliasedBuilder<SelectBuilder> Field(Expression expression)
        {
            var field = new OptionallyAliased<Expression>(expression);
            Statement.Fields.Add(field);
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
