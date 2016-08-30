using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class ConditionBuilder
        : SqlExpression<IScalar<bool>>
        , ISqlCondition
    {
        ConditionCombinator Combinator { get; }

        List<ISqlCondition> Expressions { get; } =
            new List<ISqlCondition>();

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

        public ConditionBuilder Add(ISqlCondition condition)
        {
            Expressions.Add(condition);
            return this;
        }

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
    }
}
