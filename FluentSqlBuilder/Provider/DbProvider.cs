using System.Data.Common;

namespace FluentSqlBuilder.Provider
{
    public class DbProvider
    {
        public SqlLanguage Language { get; }
        public DbProviderFactory Factory { get; }

        public DbProvider(SqlLanguage language, DbProviderFactory factory)
        {
            Language = language;
            Factory = factory;
        }
    }
}
