using System.Collections.Generic;
using System.Data.Common;
using System.Linq;

namespace FluentSqlBuilder.Detail
{
    public class ConditionBuilder
        : SqlExpression
    {
        ConditionCombinator Combinator { get; }

        List<ISqlPart> Expressions { get; } =
            new List<ISqlPart>();

        public ConditionBuilder(SqlBuilder sqlBuilder, ConditionCombinator combinator)
            : base(sqlBuilder)
        {
            Combinator = combinator;
        }

        public ConditionBuilder(SqlBuilder sqlBuilder)
            : this(sqlBuilder, ConditionCombinator.And)
        {
        }

        #region SqlExpression
        public override IEnumerable<string> Tokens =>
            Combinator.Combine(Expressions.Select(x => x.Tokens))
            .Enclose("(", ")");

        public override IEnumerable<DbParameter> Parameters =>
            Expressions.SelectMany(x => x.Parameters);
        #endregion

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

        ConditionBuilder AddExpression(params ISqlPart[] parts)
        {
            Expressions.Add(SqlPart.Concat(parts));
            return this;
        }

        public ConditionBuilder Equal(SqlExpression lhs, SqlExpression rhs)
        {
            var equal = SqlPart.FromToken("=");
            return AddExpression(lhs, equal, rhs);
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
