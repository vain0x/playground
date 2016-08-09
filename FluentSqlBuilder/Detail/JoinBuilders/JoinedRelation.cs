using System.Collections.Generic;

namespace FluentSqlBuilder.Detail
{
    public class JoinedRelation
    {
        public List<OptionallyAliased<Expression>> Relations { get; } =
            new List<OptionallyAliased<Expression>>();

        public List<Join> Joins { get; } =
            new List<Join>();
    }
}
