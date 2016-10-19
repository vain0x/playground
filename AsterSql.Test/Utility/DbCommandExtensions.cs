using System;
using System.Data;
using System.Data.Common;
using System.Linq;
using System.Text.RegularExpressions;
using static System.Threading.Interlocked;

namespace AsterSql.Test
{
    public static class DbCommandExtensions
    {
        static Regex ParameterPattern { get; } =
            new Regex(@"@\w+");

        static string IndexParameters(string sql)
        {
            var i = -1;
            return ParameterPattern.Replace(sql, m => $"@p{Increment(ref i)}");
        }

        public static string ToParameterizedString(this DbCommand command)
        {
            return IndexParameters(command.CommandText);
        }

        public static string ToEmbeddedString(this DbCommand command)
        {
            var text = command.CommandText;
            foreach (var parameter in command.Parameters.Cast<DbParameter>())
            {
                var valueString =
                    (parameter.Value == DBNull.Value || parameter.Value == null)
                        ? "null" :
                    parameter.DbType == DbType.String
                        ? $"'{parameter.Value}'"
                        : parameter.Value.ToString();
                text = text.Replace("@" + parameter.ParameterName, valueString);
            }
            return IndexParameters(text);
        }
    }
}
