using System.Collections.Generic;

namespace FluentSqlBuilder.Detail
{
    public class ConditionBuilder
    {
        private readonly ConditionCombinator _combinator;

        public List<string> Expressions { get; private set; } =
            new List<string>();

        public ConditionBuilder(ConditionCombinator combinator)
        {
            _combinator = combinator;
        }

        public ConditionBuilder()
            : this(ConditionCombinator.And)
        {
        }

        public override string ToString()
        {
            return _combinator.Concat(Expressions);
        }

        public ConditionBuilder Add(ConditionBuilder condition)
        {
            if (ReferenceEquals(_combinator, condition._combinator))
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
        private readonly ConditionBuilder _builder;
        private readonly TResult _result;

        public ConditionBuilder(ConditionBuilder builder, TResult result)
        {
            _builder = builder;
            _result = result;
        }

        public TResult Equal(string lhs, string rhs)
        {
            _builder.Equal(lhs, rhs);
            return _result;
        }
    }
}
