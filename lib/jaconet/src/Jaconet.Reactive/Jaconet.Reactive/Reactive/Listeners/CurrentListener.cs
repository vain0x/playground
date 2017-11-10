using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;

namespace Jaconet.Reactive.Listeners
{
    /// <summary>
    /// Nongeneric version of <see cref="CurrentListener{TValue}"/>.
    /// </summary>
    public interface CurrentListener
    {
        /// <summary>
        /// Gets the cancellation token.
        /// </summary>
        CancellationToken CancellationToken { get; }

        /// <summary>
        /// Invoked whenever the current produced a value.
        /// </summary>
        /// <param name="value"></param>
        void OnValueNongeneric(object value);

        /// <summary>
        /// Invoked when the current stopped exceptionally.
        /// </summary>
        /// <param name="error"></param>
        void OnError(Exception error);

        /// <summary>
        /// Invoked when the current stopped gracefully.
        /// </summary>
        void OnCompleted();
    }

    /// <summary>
    /// Represents an object which listen to a current.
    /// Equivalent to <see cref="IObserver{T}"/>.
    /// </summary>
    /// <typeparam name="TValue"></typeparam>
    public interface CurrentListener<in TValue>
        : CurrentListener
    {
        /// <summary>
        /// Invoked whenever the current produced a value.
        /// </summary>
        /// <param name="value"></param>
        void OnValue(TValue value);
    }
}
