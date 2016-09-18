using System;
using System.Collections.Generic;
using System.Data.Common;
using Optional;

namespace FluentSqlBuilder.SqlSyntax
{
    public sealed class SelectBuilder
    {
        SelectStatement Statement { get; }

        internal SelectBuilder(SelectStatement statement)
        {
            Statement = statement;
        }

        public DbCommand ToCommand() => Statement.ToCommand();

        #region Field
        public SelectBuilder Field(ScalarSqlExpression expression)
        {
            Statement.Fields.Add(expression);
            return this;
        }

        public SelectBuilder FieldAll<R>(R relation)
            where R: RelationSqlExpression, IAliasedSqlExpression
        {
            Statement.AddFieldAll(relation);
            return this;
        }
        #endregion

        #region Union
        FromlessSelectBuilder Combine(string combinator)
        {
            var combined = new CombinedSelectStatement(Statement, combinator);
            return new FromlessSelectBuilder(Statement.SqlBuilder, combined.Some());
        }

        public FromlessSelectBuilder Union()
        {
            return Combine("union");
        }
        #endregion

        public RelationSqlExpression ToRelation()
        {
            return Statement.ToRelation();
        }
    }
}
