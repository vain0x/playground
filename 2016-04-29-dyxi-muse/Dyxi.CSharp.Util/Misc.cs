using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Dyxi.CSharp.Util
{
    public class NullableUnit
    {
        private static NullableUnit _instance = null;
        public static NullableUnit Instance
        {
            get { return (_instance ?? (_instance = new NullableUnit())); }
        }
    }

    public static class IEnumerableExtensions
    {
        public static bool IsEmpty<T>(this IEnumerable<T> self)
        {
            return self.All(x => false);
        }
    }
}
