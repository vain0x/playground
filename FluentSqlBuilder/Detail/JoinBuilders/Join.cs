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
        public JoinType JoinType { get; private set; }
        public OptionallyAliased<Expression> Relation { get; private set; }

        public Join(
            JoinType joinType,
            OptionallyAliased<Expression> relation)
        {
            JoinType = joinType;
            Relation = relation;
        }
    }

    public class JoinOn
        : Join
    {
        public ConditionBuilder Condition { get; private set; }

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
        public Expression Column { get; private set; }

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
