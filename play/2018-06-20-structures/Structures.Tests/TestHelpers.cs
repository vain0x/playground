using System;
using System.Collections.Generic;
using Xunit;

namespace Xunit
{
    public static class XunitExtension
    {
        public static void Is<T>(this T actual, T expected)
        {
            Assert.Equal(expected, actual);
        }

        public static void Are<T>(this IEnumerable<T> actual, IEnumerable<T> expected)
        {
            Assert.Equal(expected, actual);
        }
    }
}
