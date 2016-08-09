using System;
using System.Collections.Generic;
using System.Linq;

namespace FluentSqlBuilder.Detail
{
    public abstract class ConditionCombinator
    {
        public abstract string Unit { get; }
        public abstract string Combinator { get; }

        public string Concat(IEnumerable<string> expressions)
        {
            return expressions.Any()
                ? string.Join(Combinator, expressions.Select(x => "(" + x + ")"))
                : Unit;
        }

        public sealed class AndConditionCombinator
            : ConditionCombinator
        {
            public override string Unit
            {
                get { return "0 = 0"; }
            }

            public override string Combinator
            {
                get { return "and"; }
            }

            private AndConditionCombinator()
            {
            }

            public static readonly AndConditionCombinator Instance =
                new AndConditionCombinator();
        }

        public sealed class OrConditionCombinator
            : ConditionCombinator
        {
            public override string Unit
            {
                get { return "0 = 1"; }
            }

            public override string Combinator
            {
                get { return "or"; }
            }

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
