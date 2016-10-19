using System.Collections.Generic;
using System.Data.Common;
using System.Linq;

namespace AsterSql.Core.SqlSyntax
{
    sealed class JoinedRelation
        : RelationSqlExpression
    {
        public RelationSqlExpression Relation { get; }
        public Join Join { get; }

        public JoinedRelation(RelationSqlExpression relation, Join join)
            : base(relation.SqlBuilder)
        {
            Relation = relation;
            Join = join;
        }

        internal override IEnumerable<SqlToken> Tokens =>
            Relation.Tokens.Concat(Join.Tokens);
    }
}
