using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class JoinedRelation
        : ISqlPart
    {
        public ISqlExpression<IRelation> Relation { get; }

        public List<Join> Joins { get; } =
            new List<Join>();

        public JoinedRelation(ISqlExpression<IRelation> relation)
        {
            Relation = relation;
        }

        #region Add
        public void Add(Join join)
        {
            Joins.Add(join);
        }
        #endregion

        #region ISqlPart
        public IEnumerable<string> Tokens
        {
            get
            {
                foreach (var token in Relation.Tokens) yield return token;

                foreach (var join in Joins)
                {
                    foreach (var token in join.Tokens) yield return token;
                }
            }
        }

        public IEnumerable<DbParameter> Parameters =>
            Enumerable.Concat(
                Relation.Parameters,
                Joins.SelectMany(j => j.Parameters)
            );
        #endregion
    }
}
