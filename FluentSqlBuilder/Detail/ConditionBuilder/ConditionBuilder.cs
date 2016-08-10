using System.Collections.Generic;

namespace FluentSqlBuilder.Detail
{
    public class ConditionBuilder
    {
        ConditionCombinator Combinator { get; }

        public List<string> Expressions { get; } =
            new List<string>();

        public ConditionBuilder(ConditionCombinator combinator)
        {
            Combinator = combinator;
        }

        public ConditionBuilder()
            : this(ConditionCombinator.And)
        {
        }

        public override string ToString()
        {
            return Combinator.Concat(Expressions);
        }

        public ConditionBuilder Add(ConditionBuilder condition)
        {
            if (ReferenceEquals(Combinator, condition.Combinator))
            {
                Expressions.AddRange(condition.Expressions);
            }
            else
            {
                Expressions.Add(condition.ToString());
            }
            return this;
        }

        public ConditionBuilder Equal(string lhs, string rhs)
        {
            Expressions.Add(string.Format("{0} = {1}", lhs, rhs));
            return this;
        }
    }

    public class ConditionBuilder<TResult>
    {
        ConditionBuilder Condition { get; }
        TResult Result { get; }

        public ConditionBuilder(ConditionBuilder builder, TResult result)
        {
            Condition = builder;
            Result = result;
        }

        public TResult Equal(string lhs, string rhs)
        {
            Condition.Equal(lhs, rhs);
            return Result;
        }
    }
}
