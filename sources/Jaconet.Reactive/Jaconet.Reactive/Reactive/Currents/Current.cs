using System;
using System.Collections.Generic;
using System.Text;
using Jaconet.Reactive.Listeners;

namespace Jaconet.Reactive.Currents
{
    /// <summary>
    /// Nongeneric version of <see cref="Current{TValue}"/>.
    /// </summary>
    public interface Current
    {
        /// <summary>
        /// Connects to the listener.
        /// </summary>
        /// <param name="listener"></param>
        void ConnectNongeneric(CurrentListener listener);
    }

    /// <summary>
    /// Represents a push-based sequence.
    /// Equivalent to <see cref="IObservable{T}"/>.
    /// </summary>
    /// <typeparam name="TValue"></typeparam>
    public interface Current<out TValue>
        : Current
    {
        /// <summary>
        /// Connects to the listener.
        /// </summary>
        /// <param name="listener"></param>
        void Connect(CurrentListener<TValue> listener);
    }
}
