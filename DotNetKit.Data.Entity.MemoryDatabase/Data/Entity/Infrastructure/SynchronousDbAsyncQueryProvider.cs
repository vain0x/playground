using System;
using System.Collections.Generic;
using System.Data.Entity.Infrastructure;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using DotNetKit.Linq;

namespace DotNetKit.Data.Entity.Infrastructure
{
    sealed class SynchronousDbAsyncQueryProvider
        : IDbAsyncQueryProvider
    {
        readonly IQueryProvider queryProvider;

        public IQueryable<TElement> CreateQuery<TElement>(Expression expression)
        {
            return new SynchronousAsyncQueryable<TElement>(queryProvider.CreateQuery<TElement>(expression));
        }

        public IQueryable CreateQuery(Expression expression)
        {
            return new SynchronousAsyncQueryable(queryProvider.CreateQuery(expression));
        }

        public TResult Execute<TResult>(Expression expression)
        {
            return queryProvider.Execute<TResult>(expression);
        }

        public object Execute(Expression expression)
        {
            return queryProvider.Execute(expression);
        }

        public Task<object> ExecuteAsync(Expression expression, CancellationToken cancellationToken)
        {
            return Task.FromResult(Execute(expression));
        }

        public Task<TResult> ExecuteAsync<TResult>(Expression expression, CancellationToken cancellationToken)
        {
            return Task.FromResult(Execute<TResult>(expression));
        }

        public SynchronousDbAsyncQueryProvider(IQueryProvider queryProvider)
        {
            this.queryProvider = queryProvider;
        }
    }
}
