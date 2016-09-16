using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public sealed class JoinedRelation
        : SqlExpression<IRelation>
    {
        public SqlExpression<IRelation> Relation { get; }
        public Join Join { get; }

        public JoinedRelation(SqlExpression<IRelation> relation, Join join)
            : base(relation.SqlBuilder)
        {
            Relation = relation;
            Join = join;
        }

        #region SqlPart
        internal override IEnumerable<string> Tokens =>
            Relation.Tokens.Concat(Join.Tokens);

        internal override IEnumerable<DbParameter> Parameters =>
            Relation.Parameters.Concat(Join.Parameters);
        #endregion
    }
}
