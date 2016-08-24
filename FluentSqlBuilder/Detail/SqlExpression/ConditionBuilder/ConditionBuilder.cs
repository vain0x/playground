using System.Collections.Generic;
using System.Data.Common;
using System.Linq;

namespace FluentSqlBuilder.Detail
{
    public class ConditionBuilder
        : SqlExpression
    {
        ConditionCombinator Combinator { get; }

        List<SqlExpression> Expressions { get; } =
            new List<SqlExpression>();

        public ConditionBuilder(SqlBuilder sqlBuilder, ConditionCombinator combinator)
            : base(sqlBuilder)
        {
            Combinator = combinator;
        }

        public ConditionBuilder(SqlBuilder sqlBuilder)
            : this(sqlBuilder, ConditionCombinator.And)
        {
        }

        public override IEnumerable<string> Tokens =>
            Combinator.Combine(Expressions.Select(x => x.Tokens))
            .Enclose("(", ")");

        public override IEnumerable<DbParameter> Parameters =>
            Expressions.SelectMany(x => x.Parameters);

        public bool IsTrivial =>
            Expressions.IsEmpty();

        public ConditionBuilder Add(ConditionBuilder condition)
        {
            if (ReferenceEquals(Combinator, condition.Combinator))
            {
                Expressions.AddRange(condition.Expressions);
            }
            else
            {
                Expressions.Add(condition);
            }
            return this;
        }

        ConditionBuilder AddSequence(params SqlExpression[] expressions)
        {
            Expressions.Add(new CompoundExpression(SqlBuilder, expressions));
            return this;
        }

        public ConditionBuilder Equal(SqlExpression lhs, SqlExpression rhs)
        {
            var equal = new AtomicExpression(SqlBuilder, "=");
            return AddSequence(lhs, equal, rhs);
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

        public TResult Equal(SqlExpression lhs, SqlExpression rhs)
        {
            Condition.Equal(lhs, rhs);
            return Result;
        }
    }
}
