namespace FluentSqlBuilder.Detail
{
    public enum JoinType
    {
        Inner,
        Cross,
        LeftOuter,
        RightOuter,
        FullOuter,
    }

    public abstract class Join
    {
        public JoinType JoinType { get; }
        public OptionallyAliased<Expression> Relation { get; }

        public Join(
            JoinType joinType,
            OptionallyAliased<Expression> relation
        )
        {
            JoinType = joinType;
            Relation = relation;
        }
    }

    public class JoinOn
        : Join
    {
        public ConditionBuilder Condition { get; }

        public JoinOn(
            JoinType joinType,
            OptionallyAliased<Expression> relation,
            ConditionBuilder condition
        )
            : base(joinType, relation)
        {
            Condition = condition;
        }
    }

    public class JoinUsing
        : Join
    {
        public Expression Column { get; }

        public JoinUsing(
            JoinType joinType,
            OptionallyAliased<Expression> relation,
            Expression column
        )
            : base(joinType, relation)
        {
            Column = column;
        }
    }
}
