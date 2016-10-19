using System;
using System.Collections;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;

namespace AsterSql.Core.SqlSyntax
{
    /// <summary>
    /// SQL文の断片を表す。
    /// </summary>
    public abstract class SqlPart
    {
        internal abstract IEnumerable<SqlToken> Tokens { get; }

        public override string ToString()
        {
            return string.Join(" ", Tokens.Select(t => t.String));
        }

        public static SqlPart FromString(string token)
        {
            return new ConcreteSqlPart(new[] { SqlToken.FromString(token) });
        }
    }

    sealed class ConcreteSqlPart
        : SqlPart
    {
        internal override IEnumerable<SqlToken> Tokens { get; }

        public ConcreteSqlPart(IEnumerable<SqlToken> tokens)
        {
            Tokens = tokens;
        }
    }
}
