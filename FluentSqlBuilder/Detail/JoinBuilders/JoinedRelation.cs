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

        public void Add(Join join)
        {
            Joins.Add(join);
        }
        #endregion
    }
}
