using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;

namespace Jaconet.Reactive.Listeners
{
    abstract class BaseCurrentListener<TValue>
        : CurrentListener<TValue>
    {
        public abstract CancellationToken CancellationToken { get; }

        public abstract void OnValue(TValue value);
        public abstract void OnError(Exception error);
        public abstract void OnCompleted();

        void CurrentListener.OnValueNongeneric(object value)
        {
            OnValue((TValue)value);
        }
    }
}
