using System;
using System.Collections.Generic;
using FluentSqlBuilder.Interface;

namespace FluentSqlBuilder.Detail
{
    public class SelectStatement
        : ManipulationStatement
    {
        public ConditionBuilder WhereCondition { get; private set; } =
            new ConditionBuilder();

        public ConditionBuilder HavingCondition { get; private set; } =
            new ConditionBuilder();

        public List<Expression> GroupKeys { get; private set; } =
            new List<Expression>();

        public List<OrderKey> OrderKeys { get; private set; } =
            new List<OrderKey>();

        public List<OptionallyAliased<Expression>> Fields { get; private set; } =
            new List<OptionallyAliased<Expression>>();
    }
}
