﻿using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class FieldlessSelectBuilder
    {
        SelectStatement Statement { get; }
        
        public FieldlessSelectBuilder(SelectStatement statement)
        {
            Statement = statement;
        }

        #region From
        public FieldlessSelectBuilder From(ISqlExpression<IRelation> relation)
        {
            Statement.Source.Add(relation);
            return this;
        }
        #endregion

        #region Join
        JoinBuilder<FieldlessSelectBuilder> Join(ISqlExpression<IRelation> relation, JoinType joinType)
        {
            return new JoinBuilder<FieldlessSelectBuilder>(Statement.SqlBuilder, Statement, joinType, relation, this);
        }

        public JoinBuilder<FieldlessSelectBuilder> Join(ISqlExpression<IRelation> relation)
        {
            return Join(relation, JoinType.Inner);
        }
        #endregion

        #region Where
        public FieldlessSelectBuilder Where(ISqlCondition condition)
        {
            Statement.WhereCondition.Add(condition);
            return this;
        }
        #endregion

        #region GroupBy
        public FieldlessSelectBuilder GroupBy<X>(ISqlExpression<IScalar<X>> expression)
        {
            Statement.GroupKeys.Add(expression);
            return this;
        }
        #endregion

        #region OrderBy
        FieldlessSelectBuilder OrderByImpl<X>(ISqlExpression<IScalar<X>> expression, OrderDirection direction)
        {
            Statement.OrderKeys.Add(new OrderKey(expression, direction));
            return this;
        }

        public FieldlessSelectBuilder OrderBy<X>(ISqlExpression<IScalar<X>> column)
        {
            return OrderByImpl(column, OrderDirection.Ascending);
        }

        public FieldlessSelectBuilder OrderByDescending<X>(ISqlExpression<IScalar<X>> column)
        {
            return OrderByImpl(column, OrderDirection.Descending);
        }
        #endregion

        #region Field
        public SelectBuilder Field<X>(ISqlExpression<IScalar<X>> expression)
        {
            Statement.Fields.Add(expression);
            return new SelectBuilder(Statement);
        }
        #endregion
    }
}
