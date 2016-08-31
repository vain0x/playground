using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using System.Text;

namespace FluentSqlBuilder.Detail
{
    static class SqlPartExtensions
    {
        internal static ISqlPart Concat(this ISqlPart lhs, ISqlPart rhs)
        {
            return SqlPart.Concat(new[] { lhs, rhs });
        }

        internal static ISqlPart Enclose(this ISqlPart part, string left, string right)
        {
            var parts = new[] { SqlPart.FromToken(left), part, SqlPart.FromToken(right) };
            return SqlPart.Concat(parts);
        }

        /// <summary>
        /// NOTE: Values are NOT quoted. Just for test.
        /// </summary>
        internal static string ToEmbeddedString(this ISqlPart part)
        {
            var parameterDictionary = part.Parameters.ToDictionary(p => p.ParameterName);
            var tokens = new List<string>();
            foreach (var token in part.Tokens)
            {
                if (token.StartsWith("@", StringComparison.CurrentCulture))
                {
                    var parameter = (DbParameter)null;
                    if (parameterDictionary.TryGetValue(token.Substring(1), out parameter))
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
                    tokens.Add(token);
                }
            }
            return tokens.Intercalate(' ');
        }
    }
}
