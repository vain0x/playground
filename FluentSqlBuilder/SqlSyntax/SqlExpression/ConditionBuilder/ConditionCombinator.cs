using System.Collections.Generic;
using System.Linq;

namespace FluentSqlBuilder.Detail
{
    public abstract class ConditionCombinator
    {
        /// <summary>
        /// 中立元
        /// </summary>
        public abstract string Neutral { get; }

        /// <summary>
        /// 吸収元
        /// </summary>
        public abstract string Absorbing { get; }

        public abstract string Combinator { get; }

        public IEnumerable<string> Combine(IEnumerable<IEnumerable<string>> expressions)
        {
            var xs = expressions.Where(x => !x.IsSingle(Neutral)).ToArray();
            return
                xs.Any(x => x.IsSingle(Absorbing))
                    ? new[] { Absorbing } :
                xs.Any()
                    ? xs.Intercalate(new[] { Combinator })
                    : new[] { Neutral };
        }

        static string TrueExpression => "0 = 0";
        static string FalseExpression => "0 = 1";

        public sealed class AndConditionCombinator
            : ConditionCombinator
        {
            public override string Neutral => TrueExpression;
            public override string Absorbing => FalseExpression;
            public override string Combinator => "and";
            
            AndConditionCombinator()
            {
            }

            public static AndConditionCombinator Instance { get; } =
                new AndConditionCombinator();
        }

        public sealed class OrConditionCombinator
            : ConditionCombinator
        {
            public override string Neutral => FalseExpression;
            public override string Absorbing => TrueExpression;
            public override string Combinator => "or";

            OrConditionCombinator()
            {
            }

            public static OrConditionCombinator Instance { get; } =
                new OrConditionCombinator();
        }

        public static ConditionCombinator And => AndConditionCombinator.Instance;
        public static ConditionCombinator Or => OrConditionCombinator.Instance;
    }
}
