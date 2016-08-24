using System.Collections.Generic;
using System.Data.Common;
using System.Linq;

namespace FluentSqlBuilder.Detail
{
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

        public static ISqlPart FromToken(string token)
        {
            return new SqlPart(new string[] { token });
        }

        public static ISqlPart Concat(IEnumerable<ISqlPart> parts)
        {
            return
                new SqlPart(
                    parts.SelectMany(p => p.Tokens),
                    parts.SelectMany(p => p.Parameters)
                );
        }
    }
}
