using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;

namespace FluentSqlBuilder.Detail
{
    /// <summary>
    /// SQL文の断片を表します。
    /// </summary>
    public interface ISqlPart
    {
        IEnumerable<string> Tokens { get; }
        IEnumerable<DbParameter> Parameters { get; }
    }

    public class SqlPart
        : ISqlPart
    {
        #region ISqlPart
        public IEnumerable<string> Tokens { get; }
        public IEnumerable<DbParameter> Parameters { get; }
        #endregion

        public SqlPart(IEnumerable<string> tokens, IEnumerable<DbParameter> parameters)
        {
            Tokens = tokens;
            Parameters = parameters;
        }

        public SqlPart(IEnumerable<string> tokens)
            : this(tokens, Enumerable.Empty<DbParameter>())
        {
        }

        public override string ToString()
        {
            return string.Join(" ", Tokens);
        }

        public static ISqlPart FromToken(string token) =>
            new SqlPart(new[] { token });

        public static ISqlPart Concat(IEnumerable<ISqlPart> parts) =>
            new SqlPart(
                parts.SelectMany(p => p.Tokens),
                parts.SelectMany(p => p.Parameters)
            );
    }
}
