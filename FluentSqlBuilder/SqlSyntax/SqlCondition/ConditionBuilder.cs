using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public sealed class ConditionBuilder
        : SqlCondition
    {
        ConditionCombinator Combinator { get; }

        List<SqlCondition> Conditions { get; } =
            new List<SqlCondition>();

        public ConditionBuilder(SqlBuilder sqlBuilder, ConditionCombinator combinator)
            : base(sqlBuilder)
        {
            Combinator = combinator;
        }

        public ConditionBuilder(SqlBuilder sqlBuilder)
            : this(sqlBuilder, ConditionCombinator.And)
        {
        }

        #region SqlPart
        internal override IEnumerable<string> Tokens
        {
            get
            {
                var tokens = Combinator.Combine(Conditions.Select(x => x.Tokens));
                return Conditions.Count > 1 ? tokens.Enclose("(", ")") : tokens;
            }
        }

        internal override IEnumerable<DbParameter> Parameters =>
            Conditions.SelectMany(x => x.Parameters);
        #endregion

        public bool IsTrivial =>
            Conditions.IsEmpty();

        internal ConditionBuilder Add(SqlCondition condition)
        {
            Conditions.Add(condition);
            return this;
        }

        internal ConditionBuilder Add(ConditionBuilder condition)
        {
            if (ReferenceEquals(Combinator, condition.Combinator))
            {
                Conditions.AddRange(condition.Conditions);
            }
            else
            {
                Conditions.Add(condition);
            }
            return this;
        }

        #region SqlCondition
        public override ConditionBuilder And(SqlCondition rhs) =>
            ReferenceEquals(Combinator, ConditionCombinator.And)
                ? Add(rhs)
                : new ConditionBuilder(SqlBuilder, ConditionCombinator.And)
                    .Add(this)
                    .Add(rhs);

        public override ConditionBuilder Or(SqlCondition rhs) =>
            ReferenceEquals(Combinator, ConditionCombinator.Or)
                ? Add(rhs)
                : new ConditionBuilder(SqlBuilder, ConditionCombinator.Or)
                    .Add(this)
                    .Add(rhs);
        #endregion
    }
}
