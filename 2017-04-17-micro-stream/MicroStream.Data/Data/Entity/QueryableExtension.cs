using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace MicroStream.Data.Entity
{
    public static class QueryableExtension
    {
        public sealed class Mutable<TValue>
        {
            public TValue value;
        }

        public static Task<Mutable<X>> FirstOrNullAsyncCore<X>(IQueryable<X> @this, CancellationToken cancellationToken)
        {
            return
                @this.Select(x => new Mutable<X>() { value = x })
                .FirstOrDefaultAsync(cancellationToken);
        }
    }
}
