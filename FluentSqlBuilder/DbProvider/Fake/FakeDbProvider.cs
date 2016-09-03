namespace FluentSqlBuilder.Provider.Fake
{
    public class FakeDbProvider
        : DbProvider
    {
        public FakeDbProvider()
            : base(new FakeSqlLanguage(), new FakeDbProviderFactory())
        {
        }
    }
}
