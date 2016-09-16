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
        internal abstract IEnumerable<string> Tokens { get; }
        internal abstract IEnumerable<DbParameter> Parameters { get; }

        public override string ToString()
        {
            return string.Join(" ", Tokens);
        }

        public static SqlPart FromToken(string token) =>
            new ConcreteSqlPart(new[] { token });

        public static SqlPart Concat(IEnumerable<SqlPart> parts) =>
            new ConcreteSqlPart(
                parts.SelectMany(p => p.Tokens),
                parts.SelectMany(p => p.Parameters)
            );
    }

    public sealed class ConcreteSqlPart
        : SqlPart
    {
        #region SqlPart
        internal override IEnumerable<string> Tokens { get; }
        internal override IEnumerable<DbParameter> Parameters { get; }
        #endregion

        public ConcreteSqlPart(IEnumerable<string> tokens, IEnumerable<DbParameter> parameters)
        {
            Tokens = tokens;
            Parameters = parameters;
        }

        public ConcreteSqlPart(IEnumerable<string> tokens)
            : this(tokens, Enumerable.Empty<DbParameter>())
        {
        }
    }
}
