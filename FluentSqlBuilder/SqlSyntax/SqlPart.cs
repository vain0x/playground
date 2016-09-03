using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;

namespace FluentSqlBuilder.Detail
{
    /// <summary>
    /// SQL文の断片を表す。
    /// </summary>
    public interface ISqlPart
    {
        IEnumerable<string> Tokens { get; }
        IEnumerable<DbParameter> Parameters { get; }
    }

    public class ConcreteSqlPart
        : ISqlPart
    {
        #region ISqlPart
        public IEnumerable<string> Tokens { get; }
        public IEnumerable<DbParameter> Parameters { get; }
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

        public override string ToString()
        {
            return string.Join(" ", Tokens);
        }
    }

    public static class SqlPart
    {
        public static ISqlPart FromToken(string token) =>
            new ConcreteSqlPart(new[] { token });

        public static ISqlPart Concat(IEnumerable<ISqlPart> parts) =>
            new ConcreteSqlPart(
                parts.SelectMany(p => p.Tokens),
                parts.SelectMany(p => p.Parameters)
            );
    }
}
