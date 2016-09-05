using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class JoinedRelation
        : SqlExpression<IRelation>
    {
        public ISqlExpression<IRelation> Relation { get; }
        public Join Join { get; }

        public JoinedRelation(ISqlExpression<IRelation> relation, Join join)
            : base(relation.SqlBuilder)
        {
            Relation = relation;
            Join = join;
        }

        #region ISqlPart
        public override IEnumerable<string> Tokens =>
            Relation.Tokens.Concat(Join.Tokens);

        public override IEnumerable<DbParameter> Parameters =>
            Relation.Parameters.Concat(Join.Parameters);
        #endregion
    }
}
