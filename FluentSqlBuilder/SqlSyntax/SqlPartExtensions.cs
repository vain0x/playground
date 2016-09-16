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
        internal static SqlPart Concat(this SqlPart lhs, SqlPart rhs)
        {
            return SqlPart.Concat(new[] { lhs, rhs });
        }

        internal static SqlPart Enclose(this SqlPart part, string left, string right)
        {
            var parts = new[] { SqlPart.FromString(left), part, SqlPart.FromString(right) };
            return SqlPart.Concat(parts);
        }

        /// <summary>
        /// NOTE: Values are NOT quoted. Just for test.
        /// </summary>
        internal static string ToEmbeddedString(this SqlPart part)
        {
            var parameterDictionary =
                part.Tokens
                .SelectMany(t => t.Parameters)
                .ToDictionary(p => p.ParameterName);

            var tokens = new List<string>();
            foreach (var token in part.Tokens)
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
    }
}
