using System;
using System.Collections;
using System.Collections.Generic;
using System.Data.Entity.Infrastructure;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Threading.Tasks;
using DotNetKit.Data.Entity.Infrastructure;

namespace DotNetKit.Linq
{
    sealed class SynchronousAsyncQueryable<TValue>
        : IQueryable<TValue>
        , IDbAsyncEnumerable<TValue>
    {
        readonly IQueryable<TValue> queryable;

        public Type ElementType => queryable.ElementType;

        public Expression Expression => queryable.Expression;

        public IQueryProvider Provider { get; }

        public IEnumerator<TValue> GetEnumerator()
        {
            return queryable.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        public IDbAsyncEnumerator<TValue> GetAsyncEnumerator()
        {
            return new SynchronousDbAsyncEnumerator<TValue>(GetEnumerator());
        }

        IDbAsyncEnumerator IDbAsyncEnumerable.GetAsyncEnumerator()
        {
            return GetAsyncEnumerator();
        }

        public SynchronousAsyncQueryable(IQueryable<TValue> queryable)
        {
            this.queryable = queryable;
            Provider = new SynchronousDbAsyncQueryProvider(queryable.Provider);
        }
    }

    sealed class SynchronousAsyncQueryable
        : IQueryable
        , IDbAsyncEnumerable
    {
        readonly IQueryable queryable;

        public Type ElementType => queryable.ElementType;

        public Expression Expression => queryable.Expression;

        public IQueryProvider Provider { get; }

        public IEnumerator GetEnumerator()
        {
            return queryable.GetEnumerator();
        }

        public IDbAsyncEnumerator GetAsyncEnumerator()
        {
            return new SynchronousDbAsyncEnumerator(GetEnumerator());
        }

        public SynchronousAsyncQueryable(IQueryable queryable)
        {
            this.queryable = queryable;
            Provider = new SynchronousDbAsyncQueryProvider(queryable.Provider);
        }
    }
}
