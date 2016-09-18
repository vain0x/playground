using System.Collections.Generic;
using System.Linq;

namespace FluentSqlBuilder.SqlSyntax
{
    sealed class ConditionCombinator
    {
        /// <summary>
        /// 中立元
        /// </summary>
        public SqlCondition Neutral { get; }

        /// <summary>
        /// 吸収元
        /// </summary>
        public SqlCondition Absorbing { get; }

        public SqlPart Combinator { get; }

        public bool IsAnd { get; }
        public bool IsOr { get; }

        /// <summary>
        /// Combines conditions with the combinator simplifying a bit.
        /// </summary>
        /// <param name="xs"></param>
        /// <returns></returns>
        public IEnumerable<SqlPart> Combine(IEnumerable<SqlCondition> xs)
        {
            return
                xs.Any(x => ReferenceEquals(x, Absorbing))
                    ? new[] { Absorbing } :
                xs.Any()
                    ? xs.Intersperse(Combinator)
                    : new[] { Neutral };
        }

        internal ConditionCombinator(
            SqlCondition neutral,
            SqlCondition absorbing,
            string combinator,
            bool isAnd
        )
        {
            Neutral = neutral;
            Absorbing = absorbing;
            Combinator = SqlPart.FromString(combinator);
            IsAnd = isAnd;
            IsOr = !isAnd;
        }
    }
}
