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

        static readonly string _trueExpression = "0 = 0";
        static readonly string _falseExpression = "0 = 1";

        public sealed class AndConditionCombinator
            : ConditionCombinator
        {
            public override string Neutral => _trueExpression;
            public override string Absorbing => _falseExpression;
            public override string Combinator => "and";
            
            private AndConditionCombinator()
            {
            }

            public static readonly AndConditionCombinator Instance =
                new AndConditionCombinator();
        }

        public sealed class OrConditionCombinator
            : ConditionCombinator
        {
            public override string Neutral => _falseExpression;
            public override string Absorbing => _trueExpression;
            public override string Combinator => "or";

            private OrConditionCombinator()
            {
            }

            public static readonly OrConditionCombinator Instance =
                new OrConditionCombinator();
        }

        public static ConditionCombinator And
        {
            get { return AndConditionCombinator.Instance; }
        }

        public static ConditionCombinator Or
        {
            get { return OrConditionCombinator.Instance; }
        }
    }
}
