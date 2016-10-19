namespace AsterSql.Core.Provider.Fake
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
