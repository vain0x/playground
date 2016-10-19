using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using System.Text;

namespace AsterSql.Core.SqlSyntax
{
    static class SqlPartExtensions
    {
        internal static SqlPart Concat(
            this SqlPart lhs,
            SqlPart rhs
        )
        {
            return new ConcreteSqlPart(lhs.Tokens.Concat(rhs.Tokens));
        }

        internal static IEnumerable<SqlToken> Enclose(
            this IEnumerable<SqlToken> part,
            string left,
            string right
        )
        {
            yield return SqlToken.FromString(left);
            foreach (var token in part) yield return token;
            yield return SqlToken.FromString(right);
        }

        /// <summary>
        /// NOTE: Values are NOT quoted. Just for test.
        /// </summary>
        internal static string ToEmbeddedString(this IEnumerable<SqlToken> part)
        {
            var parameterDictionary =
                part
                .SelectMany(t => t.Parameters)
                .ToDictionary(p => p.ParameterName);

            var tokens = new List<string>();
            foreach (var token in part)
            {
                if (token.String.StartsWith("@", StringComparison.CurrentCulture))
                {
                    var parameter = (DbParameter)null;
                    if (parameterDictionary.TryGetValue(token.String.Substring(1), out parameter))
                    {
                        if (parameter.DbType == DbType.String)
                        {
                            tokens.Add($"'{parameter.Value}'");
                        }
                        else
                        {
                            tokens.Add(parameter.Value.ToString());
                        }
                    }
                }
                else
                {
                    tokens.Add(token.String);
                }
            }
            return tokens.Intercalate(' ');
        }

        internal static string ToEmbeddedString(this SqlPart part)
        {
            return part.Tokens.ToEmbeddedString();
        }
    }
}
