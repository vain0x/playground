namespace FluentSqlBuilder.Interface
{
    public class DbProvider
    {
        public IDbLanguage Language { get; private set; }
        public IDbParameterFactory ParameterFactory { get; private set; }

        public DbProvider(IDbLanguage language, IDbParameterFactory parameterFactory)
        {
            Language = language;
            ParameterFactory = parameterFactory;
        }
    }
}
