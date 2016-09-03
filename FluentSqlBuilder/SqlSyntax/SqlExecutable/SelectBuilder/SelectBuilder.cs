using System;
using System.Collections.Generic;
using System.Data.Common;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class SelectBuilder
    {
        SelectStatement Statement { get; }

        public SelectBuilder(SelectStatement statement)
        {
            Statement = statement;
        }

        public DbCommand ToCommand() => Statement.ToCommand();

        #region Field
        public SelectBuilder Field<X>(ISqlExpression<IScalar<X>> expression)
        {
            Statement.Fields.Add(expression.Box());
            return this;
        }

        public SelectBuilder FieldAll(IAliasedSqlExpression<IRelation> relation)
        {
            Statement.AddFieldAll(relation);
            return this;
        }
        #endregion

        #region Union
        public FromlessSelectBuilder Union()
        {
            throw new NotImplementedException();
        }
        #endregion

        public ISqlExpression<IRelation> ToRelation()
        {
            return
                new CompoundExpression<IRelation>(
                    Statement.SqlBuilder,
                    Statement.Enclose("(", ")")
                );
        }
    }
}
