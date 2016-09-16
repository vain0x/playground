using System;
using System.Collections.Generic;
using System.Data.Common;
using Optional;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public sealed class SelectBuilder
    {
        SelectStatement Statement { get; }

        public SelectBuilder(SelectStatement statement)
        {
            Statement = statement;
        }

        public DbCommand ToCommand() => Statement.ToCommand();

        #region Field
        public SelectBuilder Field<X>(SqlExpression<IScalar<X>> expression)
        {
            Statement.Fields.Add(expression);
            return this;
        }

        public SelectBuilder FieldAll(AliasedSqlExpression<IRelation> relation)
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

        public SqlExpression<IRelation> ToRelation()
        {
            return Statement.ToRelation();
        }
    }
}
