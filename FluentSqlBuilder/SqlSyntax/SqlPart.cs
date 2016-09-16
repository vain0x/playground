using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;

namespace FluentSqlBuilder.Detail
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

        public static SqlPart Singleton(SqlToken token) =>
            new ConcreteSqlPart(new[] { token });

        public static SqlPart FromString(string token) =>
            Singleton(SqlToken.FromString(token));

        public static SqlPart Concat(IEnumerable<SqlPart> parts) =>
            new ConcreteSqlPart(parts.SelectMany(part => part.Tokens));
    }

    public sealed class ConcreteSqlPart
        : SqlPart
    {
        #region SqlPart
        internal override IEnumerable<SqlToken> Tokens { get; }
        #endregion

        public ConcreteSqlPart(IEnumerable<SqlToken> tokens)
        {
            Tokens = tokens;
        }
    }
}
