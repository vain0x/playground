namespace FluentSqlBuilder.Detail
{
    public class FieldlessSelectBuilder
        : InternalBuilder
    {
        SelectStatement Statement { get; }
        
        public FieldlessSelectBuilder(SqlBuilder sqlBuilder, SelectStatement statement)
            : base(sqlBuilder)
        {
            Statement = statement;
        }

        #region From
        public OptionallyAliasedBuilder<FieldlessSelectBuilder> From(Expression relation)
        {
            var aliased = Statement.Source.Add(relation);
            return OptionallyAliasedBuilder.Create(this, aliased);
        }
        #endregion

        #region Join
        OptionallyAliasedBuilder<JoinBuilder<FieldlessSelectBuilder>> Join(Expression relation, JoinType joinType)
        {
            var aliased = new OptionallyAliased<Expression>(relation);
            var builder = new JoinBuilder<FieldlessSelectBuilder>(SqlBuilder, Statement, joinType, aliased, this);
            return OptionallyAliasedBuilder.Create(builder, aliased);
        }

        public OptionallyAliasedBuilder<JoinBuilder<FieldlessSelectBuilder>> Join(Expression relation)
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
        public FieldlessSelectBuilder GroupBy(Expression expression)
        {
            Statement.GroupKeys.Add(expression);
            return this;
        }
        #endregion

        #region OrderBy
        FieldlessSelectBuilder OrderByImpl(Expression expression, OrderDirection direction)
        {
            Statement.OrderKeys.Add(new OrderKey(expression, direction));
            return this;
        }

        public FieldlessSelectBuilder OrderBy(string columnName)
        {
            return OrderByImpl(SqlBuilder.Column(columnName), OrderDirection.Ascending);
        }
        #endregion

        #region Field
        public OptionallyAliasedBuilder<SelectBuilder> Field(Expression expression)
        {
            var field = new OptionallyAliased<Expression>(expression);
            Statement.Fields.Add(field);
            var builder = new SelectBuilder(Statement);
            return OptionallyAliasedBuilder.Create(builder, field);
        }
        #endregion
    }
}
