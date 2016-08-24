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
        public FieldlessSelectBuilder From(SqlExpression relation)
        {
            Statement.Source.Add(relation);
            return this;
        }
        #endregion

        #region Join
        JoinBuilder<FieldlessSelectBuilder> Join(SqlExpression relation, JoinType joinType)
        {
            return new JoinBuilder<FieldlessSelectBuilder>(Statement.SqlBuilder, Statement, joinType, relation, this);
        }

        public JoinBuilder<FieldlessSelectBuilder> Join(SqlExpression relation)
        {
            return Join(relation, JoinType.Inner);
        }
        #endregion

        #region Where
        public ConditionBuilder<FieldlessSelectBuilder> Where()
        {
            return new ConditionBuilder<FieldlessSelectBuilder>(Statement.WhereCondition, this);
        }

        public FieldlessSelectBuilder Where(ConditionBuilder condition)
        {
            Statement.WhereCondition.Add(condition);
            return this;
        }
        #endregion

        #region GroupBy
        public FieldlessSelectBuilder GroupBy(SqlExpression expression)
        {
            Statement.GroupKeys.Add(expression);
            return this;
        }
        #endregion

        #region OrderBy
        FieldlessSelectBuilder OrderByImpl(SqlExpression expression, OrderDirection direction)
        {
            Statement.OrderKeys.Add(new OrderKey(expression, direction));
            return this;
        }

        public FieldlessSelectBuilder OrderBy(SqlExpression column)
        {
            return OrderByImpl(column, OrderDirection.Ascending);
        }

        public FieldlessSelectBuilder OrderByDescending(SqlExpression column)
        {
            return OrderByImpl(column, OrderDirection.Descending);
        }
        #endregion

        #region Field
        public SelectBuilder Field(SqlExpression expression)
        {
            Statement.Fields.Add(expression);
            return new SelectBuilder(Statement);
        }
        #endregion
    }
}
