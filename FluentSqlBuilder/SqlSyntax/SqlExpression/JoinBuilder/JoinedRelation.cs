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

        internal override IEnumerable<SqlToken> Tokens =>
            Relation.Tokens.Concat(Join.Tokens);
    }
}
