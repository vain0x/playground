using System;
using System.Text.RegularExpressions;

namespace AsterSql.Data.Fake
{
    public class FakeSqlLanguage
        : SqlLanguage
    {
        static readonly Regex _identifier =
            new Regex(@"^[a-zA-Z_]\w*$");

        public override bool IsRawIdentifier(string identifier)
        {
            return _identifier.IsMatch(identifier);
        }

        public override string QuoteIdentifier(string identifier)
        {
            if (!IsRawIdentifier(identifier))
            {
                throw new ArgumentException(nameof(identifier));
            }
            return $"`{identifier}`";
        }

        public override string QualifyIdentifier(string qualifier, string identifier)
        {
            if (!IsRawIdentifier(qualifier))
            {
                throw new ArgumentException(nameof(qualifier));
            }
            if (!IsRawIdentifier(identifier))
            {
                throw new ArgumentException(nameof(identifier));
            }
            return $"{QuoteIdentifier(qualifier)}.{QuoteIdentifier(identifier)}";
        }

        public override string BuildWildmark(string tableAlias)
        {
            return $"{QuoteIdentifier(tableAlias)}.*";
        }
    }
}
