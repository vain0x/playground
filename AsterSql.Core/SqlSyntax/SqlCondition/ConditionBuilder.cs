using System.Collections.Generic;
using System.Data.Common;
using System.Linq;

namespace AsterSql.Core.SqlSyntax
{
    sealed class ConditionBuilder
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
            : this(sqlBuilder, sqlBuilder.SqlConditionConstant.And)
        {
        }

        #region Tokens
        IEnumerable<SqlPart> TokenSeqSeq
        {
            get
            {
                var parts = Combinator.Combine(Conditions);
                return
                    Conditions.Count > 1
                        ? parts.Enclose(SqlPart.FromString("("), SqlPart.FromString(")"))
                        : parts;
            }
        }

        internal override IEnumerable<SqlToken> Tokens =>
            TokenSeqSeq.SelectMany(part => part.Tokens);
        #endregion

        public bool IsTrivial =>
            Conditions.IsEmpty();

        internal ConditionBuilder Add(SqlCondition condition)
        {
            if (condition != Combinator.Neutral)
            {
                Conditions.Add(condition);
            }

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
        public override SqlCondition And(SqlCondition rhs) =>
            Combinator.IsAnd
                ? Add(rhs)
                : SqlBuilder.And().And(this).And(rhs);

        public override SqlCondition Or(SqlCondition rhs) =>
            Combinator.IsOr
                ? Add(rhs)
                : SqlBuilder.Or().Or(this).Or(rhs);
        #endregion
    }
}
