namespace FluentSqlBuilder
{
    public class DbmsDialect
    {
        public ISqlLanguage Language { get; }
        public IDbParameterFactory ParameterFactory { get; }

        public DbmsDialect(ISqlLanguage language, IDbParameterFactory parameterFactory)
        {
            Language = language;
            ParameterFactory = parameterFactory;
        }
    }
}
