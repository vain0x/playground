using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;

namespace Jaconet.Reactive.Listeners
{
    abstract class BaseOperatorCurrentListener<TSource, TTarget>
        : CurrentListener<TSource>
    {
        public abstract void OnValue(TSource value);
        protected abstract void OnErrorCore(Exception e);
        protected abstract void OnCompletedCore();

        protected CancellationTokenSource CancellationTokenSource { get; }
        protected CurrentListener<TTarget> Listener { get; }

        public CancellationToken CancellationToken =>
            CancellationTokenSource.Token;

        public void OnError(Exception error)
        {
            try
            {
                OnErrorCore(error);
            }
            finally
            {
                CancellationTokenSource.Cancel();
            }
        }

        public void OnCompleted()
        {
            try
            {
                OnCompletedCore();
            }
            finally
            {
                CancellationTokenSource.Cancel();
            }
        }

        void CurrentListener.OnValueNongeneric(object value)
        {
            OnValue((TSource)value);
        }

        protected
            BaseOperatorCurrentListener(
                CurrentListener<TTarget> listener,
                CancellationTokenSource cts
            )
        {
            Listener = listener;
            CancellationTokenSource = cts;
        }
    }
}
