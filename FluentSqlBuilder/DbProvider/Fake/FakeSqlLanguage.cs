using System;
using System.Text.RegularExpressions;

namespace FluentSqlBuilder.Provider.Fake
{
    public class FakeSqlLanguage
        : SqlLanguage
    {
        static readonly Regex _identifier =
            new Regex(@"^[a-zA-Z_]\w*$");

        public override bool IsIdentifier(string identifier)
        {
            return _identifier.IsMatch(identifier);
        }

        public override string QuoteIdentifier(string identifier)
        {
            return $"`{identifier}`";
        }

        public override string QualifyIdentifier(string qualifier, string identifier)
        {
            return $"{qualifier}.{identifier}";
        }

        public override string BuildWildmark(string qualifier)
        {
            if (!IsIdentifier(qualifier))
            {
                throw new ArgumentException(qualifier);
            }
            return $"`{qualifier}`.*";
        }
    }
}
