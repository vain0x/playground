using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using Jaconet.Reactive.Listeners;

namespace Jaconet.Reactive.Currents
{
    abstract class BaseOperatorCurrent<TValue>
        : Current<TValue>
    {
        protected abstract void
            ConnectCore(
                CurrentListener<TValue> listener,
                CancellationTokenSource cts
            );

        public void Connect(CurrentListener<TValue> listener)
        {
            listener.CancellationToken.ThrowIfCancellationRequested();

            var cts = new CancellationTokenSource();
            var ctr = listener.CancellationToken.Register(cts.Cancel);
            cts.Token.Register(ctr.Dispose);

            ConnectCore(listener, cts);
        }

        void Current.ConnectNongeneric(CurrentListener listener)
        {
            Connect(listener.Box<TValue>());
        }
    }
}
