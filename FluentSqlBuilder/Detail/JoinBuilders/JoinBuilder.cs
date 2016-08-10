namespace FluentSqlBuilder.Detail
{
    public class JoinBuilder<TResult>
        : InternalBuilder
    {
        ManipulationStatement Statement { get; }
        JoinType JoinType { get; }
        OptionallyAliased<Expression> Relation { get; }
        TResult Result { get; }

        public JoinBuilder(
            SqlBuilder sqlBuilder,
            ManipulationStatement statement,
            JoinType joinType,
            OptionallyAliased<Expression> relation,
            TResult result
        )
            : base(sqlBuilder)
        {
            Statement = statement;
            JoinType = joinType;
            Relation = relation;
            Result = result;
        }

        public TResult On(ConditionBuilder condition)
        {
            Statement.Source.Add(new JoinOn(JoinType, Relation, condition));
            return Result;
        }

        #region Using
        public TResult Using(Expression column)
        {
            Statement.Source.Add(new JoinUsing(JoinType, Relation, column));
            return Result;
        }
        
        public TResult Using(string columnName)
        {
            return Using(SqlBuilder.Column(columnName));
        }
        #endregion
    }
}
