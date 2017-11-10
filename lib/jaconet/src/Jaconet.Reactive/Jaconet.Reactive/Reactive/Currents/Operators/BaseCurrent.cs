using System;
using System.Collections.Generic;
using System.Text;
using Jaconet.Reactive.Listeners;

namespace Jaconet.Reactive.Currents
{
    abstract class BaseCurrent<TValue>
        : Current<TValue>
    {
        public abstract void Connect(CurrentListener<TValue> listener);

        void Current.ConnectNongeneric(CurrentListener listener)
        {
            Connect(listener.Box<TValue>());
        }
    }
}
