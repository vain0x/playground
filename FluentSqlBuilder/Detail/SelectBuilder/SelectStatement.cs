using System.Collections.Generic;

namespace FluentSqlBuilder.Detail
{
    public class SelectStatement
        : ManipulationStatement
    {
        public ConditionBuilder WhereCondition { get; } =
            new ConditionBuilder();

        public ConditionBuilder HavingCondition { get; } =
            new ConditionBuilder();

        public List<Expression> GroupKeys { get; } =
            new List<Expression>();

        public List<OrderKey> OrderKeys { get; } =
            new List<OrderKey>();

        public List<OptionallyAliased<Expression>> Fields { get; } =
            new List<OptionallyAliased<Expression>>();
    }
}
