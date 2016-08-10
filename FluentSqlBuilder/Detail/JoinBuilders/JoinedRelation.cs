using System.Collections.Generic;

namespace FluentSqlBuilder.Detail
{
    public class JoinedRelation
    {
        public List<OptionallyAliased<Expression>> Relations { get; } =
            new List<OptionallyAliased<Expression>>();

        public List<Join> Joins { get; } =
            new List<Join>();

        #region Add
        public OptionallyAliased<Expression> Add(Expression relation)
        {
            var aliased = new OptionallyAliased<Expression>(relation);
            Relations.Add(aliased);
            return aliased;
        }

        public OptionallyAliased<Expression> Add(
            Expression relation,
            JoinType joinType,
            ConditionBuilder condition
        )
        {
            var aliased = new OptionallyAliased<Expression>(relation);
            var joinOn = new JoinOn(joinType, aliased, condition);
            Joins.Add(joinOn);
            return aliased;
        }

        public OptionallyAliased<Expression> Add(
            Expression relation,
            JoinType joinType,
            Expression column
        )
        {
            var aliased = new OptionallyAliased<Expression>(relation);
            var joinUsing = new JoinUsing(joinType, aliased, column);
            Joins.Add(joinUsing);
            return aliased;
        }
        #endregion
    }
}
