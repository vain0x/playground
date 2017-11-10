using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;

namespace Jaconet.Reactive.Listeners
{
    sealed class AnonymousCurrentListener<TValue>
        : CurrentListener<TValue>
    {
        readonly Action<TValue> onValue;
        readonly Action<Exception> onError;
        readonly Action onCompleted;

        public CancellationToken CancellationToken { get; }

        public void OnValue(TValue value)
        {
            onValue(value);
        }

        void CurrentListener.OnValueNongeneric(object value)
        {
            OnValue((TValue)value);
        }

        public void OnError(Exception error)
        {
            onError(error);
        }

        public void OnCompleted()
        {
            onCompleted();
        }

        public
            AnonymousCurrentListener(
                Action<TValue> onValue,
                Action<Exception> onError,
                Action onCompleted,
                CancellationToken cancellationToken
            )
        {
            CancellationToken = cancellationToken;
            this.onValue = onValue;
            this.onError = onError;
            this.onCompleted = onCompleted;
        }
    }
}
