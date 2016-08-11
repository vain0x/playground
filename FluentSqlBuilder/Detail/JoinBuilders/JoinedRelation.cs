using System.Collections.Generic;
using System.Data.Common;
using System.Linq;

namespace FluentSqlBuilder.Detail
{
    public class JoinedRelation
        : ISqlPart
    {
        public List<SqlExpression> Relations { get; } =
            new List<SqlExpression>();

        public List<Join> Joins { get; } =
            new List<Join>();

        #region Add
        public void Add(SqlExpression relation)
        {
            Relations.Add(relation);
        }

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
                var relationTokens =
                    Relations
                    .Select(relation => relation.Tokens)
                    .Intercalate(new[] { "," });
                foreach (var token in relationTokens) yield return token;

                foreach (var join in Joins)
                {
                    foreach (var token in join.Tokens) yield return token;
                }
            }
        }

        public IEnumerable<DbParameter> Parameters =>
            Enumerable.Concat(
                Relations.SelectMany(r => r.Parameters),
                Joins.SelectMany(j => j.Parameters)
            );
        #endregion
    }
}
