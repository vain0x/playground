using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FluentSqlBuilder.SqlSyntax
{
    sealed class SqlConditionConstant
    {
        public SqlCondition True { get; }
        public SqlCondition False { get; }
        public ConditionCombinator And { get; }
        public ConditionCombinator Or { get; }

        public static string TrueExpression => "0 = 0";
        public static string FalseExpression => "0 = 1";

        public SqlConditionConstant(SqlBuilder sqlBuilder)
        {
            True =
                new AtomicSqlCondition(
                    sqlBuilder,
                    SqlPart.FromString(TrueExpression)
               );
            False =
                new AtomicSqlCondition(
                    sqlBuilder,
                    SqlPart.FromString(FalseExpression)
                );

            And = new ConditionCombinator(True, False, "and", isAnd: true);
            Or = new ConditionCombinator(False, True, "or", isAnd: false);
        }
    }
}
