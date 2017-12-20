using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;

namespace Jaconet.Reactive.Listeners
{
    sealed class BoxCurrentListener<TValue>
        : CurrentListener<TValue>
        , IEquatable<CurrentListener>
    {
        readonly CurrentListener listener;

        public CancellationToken CancellationToken =>
            listener.CancellationToken;

        public void OnValue(TValue value)
        {
            listener.OnValueNongeneric(value);
        }

        public void OnValueNongeneric(object value)
        {
            listener.OnValueNongeneric(value);
        }

        public void OnError(Exception error)
        {
            listener.OnError(error);
        }

        public void OnCompleted()
        {
            listener.OnCompleted();
        }

        #region Equality
        public bool Equals(CurrentListener other)
        {
            if (ReferenceEquals(this, other)) return true;
            if (ReferenceEquals(other, null)) return false;
            return other.Equals(listener);
        }

        public override bool Equals(object obj)
        {
            return Equals(obj as CurrentListener);
        }

        public override int GetHashCode()
        {
            return EqualityComparer<CurrentListener>.Default.GetHashCode(listener);
        }
        #endregion

        public BoxCurrentListener(CurrentListener listener)
        {
            this.listener = listener;
        }
    }
}
