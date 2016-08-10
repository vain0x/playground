namespace FluentSqlBuilder.Detail
{
    public class FieldlessSelectBuilder
        : InternalBuilder
    {
        readonly SelectStatement _statement;
        
        public FieldlessSelectBuilder(SqlBuilder sqlBuilder, SelectStatement statement)
            : base(sqlBuilder)
        {
            _statement = statement;
        }

        #region From
        public OptionallyAliasedBuilder<FieldlessSelectBuilder> From(Expression relation)
        {
            var aliased = _statement.Source.Add(relation);
            return OptionallyAliasedBuilder.Create(this, aliased);
        }
        #endregion

        #region Join
        OptionallyAliasedBuilder<JoinBuilder<FieldlessSelectBuilder>> Join(Expression relation, JoinType joinType)
        {
            var aliased = new OptionallyAliased<Expression>(relation);
            var builder = new JoinBuilder<FieldlessSelectBuilder>(SqlBuilder, _statement, joinType, aliased, this);
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
            return new ConditionBuilder<FieldlessSelectBuilder>(_statement.WhereCondition, this);
        }

        public FieldlessSelectBuilder Where(ConditionBuilder condition)
        {
            _statement.WhereCondition.Add(condition);
            return this;
        }
        #endregion

        #region GroupBy
        public FieldlessSelectBuilder GroupBy(Expression expression)
        {
            _statement.GroupKeys.Add(expression);
            return this;
        }
        #endregion

        #region OrderBy
        FieldlessSelectBuilder OrderByImpl(Expression expression, OrderDirection direction)
        {
            _statement.OrderKeys.Add(new OrderKey(expression, direction));
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
            _statement.Fields.Add(field);
            var builder = new SelectBuilder(_statement);
            return OptionallyAliasedBuilder.Create(builder, field);
        }
        #endregion
    }
}
