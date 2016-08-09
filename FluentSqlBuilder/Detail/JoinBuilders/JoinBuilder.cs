namespace FluentSqlBuilder.Detail
{
    public class JoinBuilder<TResult>
        : InternalBuilder
    {
        private readonly ManipulationStatement _statement;
        private readonly JoinType _joinType;
        private readonly OptionallyAliased<Expression> _relation;
        private readonly TResult _result;

        public JoinBuilder(
            SqlBuilder sqlBuilder,
            ManipulationStatement statement,
            JoinType joinType,
            OptionallyAliased<Expression> relation,
            TResult result
            )
            : base(sqlBuilder)
        {
            _statement = statement;
            _joinType = joinType;
            _relation = relation;
            _result = result;
        }

        public TResult On(ConditionBuilder condition)
        {
            _statement.Source.Joins.Add(new JoinOn(_joinType, _relation, condition));
            return _result;
        }

        #region Using
        public TResult Using(Expression column)
        {
            _statement.Source.Joins.Add(new JoinUsing(_joinType, _relation, column));
            return _result;
        }
        
        public TResult Using(string columnName)
        {
            return Using(SqlBuilder.Column(columnName));
        }
        #endregion
    }
}
