using Xunit;

namespace AsterSql.Test
{
    static class AssertExtensions
    {
        public static void ShouldEqual<X>(this X actual, X expected)
        {
            Assert.Equal(expected, actual);
        }
    }
}
