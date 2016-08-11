using System.Data.Common;

namespace FluentSqlBuilder.Provider
{
    public class DbProvider
    {
        public ISqlLanguage Language { get; }
        public DbProviderFactory Factory { get; }

        public DbProvider(ISqlLanguage language, DbProviderFactory factory)
        {
            Language = language;
            Factory = factory;
        }
    }
}
