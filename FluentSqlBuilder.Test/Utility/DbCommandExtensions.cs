using System;
using System.Data.Common;
using System.Text.RegularExpressions;
using static System.Threading.Interlocked;

namespace FluentSqlBuilder.Test
{
    public static class DbCommandExtensions
    {
        static Regex ParameterPattern { get; } =
            new Regex(@"@\w+");

        public static string ToParameterizedString(this DbCommand command)
        {
            var i = -1;
            return
                ParameterPattern.Replace(
                    command.CommandText,
                    m => $"@p{Increment(ref i)}"
                );
        }
    }
}
