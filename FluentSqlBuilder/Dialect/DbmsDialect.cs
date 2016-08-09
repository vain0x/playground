namespace FluentSqlBuilder
{
    public class DbmsDialect
    {
        public ISqlLanguage Language { get; private set; }
        public IDbParameterFactory ParameterFactory { get; private set; }

        public DbmsDialect(ISqlLanguage language, IDbParameterFactory parameterFactory)
        {
            Language = language;
            ParameterFactory = parameterFactory;
        }
    }
}
