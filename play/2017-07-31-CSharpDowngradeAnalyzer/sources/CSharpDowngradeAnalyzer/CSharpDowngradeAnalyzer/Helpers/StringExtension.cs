using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharpDowngradeAnalyzer
{
    public static class StringExtension
    {
        public static string RemoveSuffix(this string @this, string suffix)
        {
            return
                @this.EndsWith(suffix)
                    ? @this.Substring(0, @this.Length - suffix.Length)
                    : @this;
        }
    }
}
