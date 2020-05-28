using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharpDowngradeAnalyzer
{
    public static class EnumerableExtension
    {
        public static IEnumerable<X> TakeLast1<X>(this IEnumerable<X> @this)
        {
            // Naive impl.

            var last = default(X);
            var exists = false;

            foreach (var x in @this)
            {
                last = x;
                exists = true;
            }

            return exists ? new[] { last } : Enumerable.Empty<X>();
        }

        public static string Intercalate(this IEnumerable<string> @this, string separator)
        {
            return string.Join(separator, @this);
        }
    }
}
