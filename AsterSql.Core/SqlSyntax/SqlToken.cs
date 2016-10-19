using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FluentSqlBuilder.SqlSyntax
{
    /// <summary>
    /// Represents a token of some SQL statement.
    /// (あるSQL文の1個の字句を表す。)
    /// </summary>
    public sealed class SqlToken
    {
        public string String { get; }
        public IEnumerable<DbParameter> Parameters { get; }

        public override string ToString()
        {
            return String;
        }

        SqlToken(string @string, IEnumerable<DbParameter> parameters)
        {
            String = @string;
            Parameters = parameters;
        }

        public static SqlToken Create(string @string, IEnumerable<DbParameter> parameters)
        {
            return new SqlToken(@string, parameters);
        }

        public static SqlToken FromString(string @string)
        {
            return Create(@string, Enumerable.Empty<DbParameter>());
        }
    }
}
