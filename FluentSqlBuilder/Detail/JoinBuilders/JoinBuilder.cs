namespace FluentSqlBuilder.Detail
{
    public class JoinBuilder<TResult>
    {
        SqlBuilder SqlBuilder { get; }
        IRelationalQueryOrCommand Statement { get; }
        JoinType JoinType { get; }
        SqlExpression Relation { get; }
        TResult Result { get; }

        public JoinBuilder(
            SqlBuilder sqlBuilder,
            IRelationalQueryOrCommand statement,
            JoinType joinType,
            SqlExpression relation,
            TResult result
        )
        {
            SqlBuilder = sqlBuilder;
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

        public TResult Using(SqlExpression column)
        {
            Statement.Source.Add(new JoinUsing(JoinType, Relation, column));
            return Result;
        }
        
        public TResult Using(string columnName)
        {
            return Using(SqlBuilder.Column(columnName));
        }
    }
}
