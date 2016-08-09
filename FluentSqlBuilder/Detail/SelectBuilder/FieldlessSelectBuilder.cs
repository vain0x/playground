using System;

namespace FluentSqlBuilder.Detail
{
    public class FieldlessSelectBuilder
        : InternalBuilder
    {
        private readonly SelectStatement _statement;
        
        public FieldlessSelectBuilder(SqlBuilder sqlBuilder, SelectStatement statement)
            : base(sqlBuilder)
        {
            _statement = statement;
        }

        #region From
        public OptionallyAliasedBuilder<FieldlessSelectBuilder> From(string tableName)
        {
            var aliased = new OptionallyAliased<Expression>(SqlBuilder.Table(tableName));
            _statement.Source.Relations.Add(aliased);
            return OptionallyAliasedBuilder.Create(this, aliased);
        }
        #endregion

        #region Join
        public OptionallyAliasedBuilder<JoinBuilder<FieldlessSelectBuilder>> Join(string tableName)
        {
            var relation = new OptionallyAliased<Expression>(SqlBuilder.Table(tableName));
            var builder = new JoinBuilder<FieldlessSelectBuilder>(SqlBuilder, _statement, JoinType.Inner, relation, this);
            return OptionallyAliasedBuilder.Create(builder, relation);
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
        private FieldlessSelectBuilder OrderByImpl(Expression expression, OrderDirection direction)
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
