using FluentSqlBuilder.Public;

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

        public JoinBuilder<FieldlessSelectBuilder> Join(ISqlExpression<IRelation> relation) =>
            Join(relation, JoinType.Inner);
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
            Statement.GroupKeys.Add(expression.Box());
            return this;
        }
        #endregion

        #region OrderBy
        FieldlessSelectBuilder OrderByImpl<X>(ISqlExpression<IScalar<X>> expression, OrderDirection direction)
        {
            Statement.OrderKeys.Add(new OrderKey(expression.Box(), direction));
            return this;
        }

        public FieldlessSelectBuilder OrderBy<X>(ISqlExpression<IScalar<X>> column) =>
            OrderByImpl(column, OrderDirection.Ascending);

        public FieldlessSelectBuilder OrderByDescending<X>(ISqlExpression<IScalar<X>> column) =>
            OrderByImpl(column, OrderDirection.Descending);
        #endregion

        #region Field
        public SelectBuilder Field<X>(ISqlExpression<IScalar<X>> expression)
        {
            Statement.Fields.Add(expression.Box());
            return new SelectBuilder(Statement);
        }
        #endregion
    }
}
