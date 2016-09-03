using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FluentSqlBuilder.Detail
{
    public static class Lazy
    {
        public static Lazy<X> Create<X>(Func<X> f)
        {
            return new Lazy<X>(f);
        }
    }
}
