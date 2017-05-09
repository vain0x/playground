using System;
using System.Collections;
using System.Collections.Generic;
using System.Data.Entity.Infrastructure;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using DotNetKit.Threading.Tasks;

namespace DotNetKit.Data.Entity.Infrastructure
{
    sealed class SynchronousDbAsyncEnumerator<TValue>
        : IDbAsyncEnumerator<TValue>
    {
        readonly IEnumerator<TValue> enumerator;

        public TValue Current => enumerator.Current;

        object IDbAsyncEnumerator.Current => Current;

        public Task<bool> MoveNextAsync(CancellationToken cancellationToken)
        {
            return BooleanTaskModule.FromResult(enumerator.MoveNext());
        }

        public void Dispose()
        {
            enumerator.Dispose();
        }

        public SynchronousDbAsyncEnumerator(IEnumerator<TValue> enumerator)
        {
            this.enumerator = enumerator;
        }
    }

    sealed class SynchronousDbAsyncEnumerator
        : IDbAsyncEnumerator
    {
        readonly IEnumerator enumerator;

        public object Current => Current;

        public Task<bool> MoveNextAsync(CancellationToken cancellationToken)
        {
            return BooleanTaskModule.FromResult(enumerator.MoveNext());
        }

        public void Dispose()
        {
            (enumerator as IDisposable)?.Dispose();
        }

        public SynchronousDbAsyncEnumerator(IEnumerator enumerator)
        {
            this.enumerator = enumerator;
        }
    }
}
