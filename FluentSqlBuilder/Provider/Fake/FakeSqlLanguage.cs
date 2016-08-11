using System.Text.RegularExpressions;

namespace FluentSqlBuilder.Provider.Fake
{
    public class FakeSqlLanguage
        : ISqlLanguage
    {
        static readonly string _identifierPattern =
            @"[a-zA-Z_]\w*";

        static readonly string _escapedIdentifierPattern =
            $@"{_identifierPattern}|`{_identifierPattern}`";

        static readonly Regex _qualifiedIdentifier =
            new Regex($@"^({_escapedIdentifierPattern}\.)?{_escapedIdentifierPattern}$");

        public bool IsTableName(string word)
        {
            return _qualifiedIdentifier.IsMatch(word);
        }

        public bool IsColumnName(string word)
        {
            return IsTableName(word);
        }

        public string QualifyTableName(string qualifier, string tableName)
        {
            return $"{qualifier}.{tableName}";
        }

        public string EscapeTableName(string tableName)
        {
            return $"`{tableName}`";
        }

        public string QualifyColumnName(string qualifier, string columnName)
        {
            return QualifyTableName(qualifier, columnName);
        }

        public string EscaleColumnName(string columnName)
        {
            return EscapeTableName(columnName);
        }
    }
}
