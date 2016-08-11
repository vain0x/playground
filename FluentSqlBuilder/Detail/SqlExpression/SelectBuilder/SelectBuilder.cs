using System;
using System.Collections.Generic;
using System.Data.Common;

namespace FluentSqlBuilder.Detail
{
    public class SelectBuilder
        : ISqlPart
    {
        SelectStatement Statement { get; }

        public SelectBuilder(SelectStatement statement)
        {
            Statement = statement;
        }

        public IEnumerable<string> Tokens
        {
            get
            {

            }
        }

        #region Field
        public OptionallyAliasedBuilder<SelectBuilder> Field(SqlExpression expression)
        {
            var field = new OptionallyAliasedExpression(expression);
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
