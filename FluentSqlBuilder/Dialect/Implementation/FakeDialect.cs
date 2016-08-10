using System.Data;
using System.Data.Common;
using System.Diagnostics;
using System.Text.RegularExpressions;

namespace FluentSqlBuilder
{
    [DebuggerDisplay("{\"{ParameterName}\" = {Value}: {DbType}")]
    public class FakeDbParameter
        : DbParameter
    {
        public override DbType DbType { get; set; }
        public override ParameterDirection Direction { get; set; }
        public override bool IsNullable { get; set; }
        public override string ParameterName { get; set; }
        public override int Size { get; set; }
        public override string SourceColumn { get; set; }
        public override bool SourceColumnNullMapping { get; set;}
        public override object Value { get; set; }

        public override void ResetDbType()
        {
            DbType = DbType.String;
        }
    }

    public class FakeDbParameterFactory
        : IDbParameterFactory
    {
        public DbParameter Create(string name, DbType type, object value)
        {
            return
                new FakeDbParameter()
                {
                    ParameterName = name,
                    DbType = type,
                    Value = value
                };
        }
    }

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

    public class FakeDialect
        : DbmsDialect
    {
        public FakeDialect()
        : base(new FakeSqlLanguage(), new FakeDbParameterFactory())
        {
        }
    }
}
