using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;

namespace Jaconet.Reactive.Listeners
{
    /// <summary>
    /// Provides static methods.
    /// </summary>
    public static class CurrentListenerModule
    {
        /// <summary>
        /// Creates a current listener.
        /// </summary>
        /// <typeparam name="X"></typeparam>
        /// <param name="onValue"></param>
        /// <param name="onError"></param>
        /// <param name="onCompleted"></param>
        /// <param name="cancellationToken"></param>
        /// <returns></returns>
        public static CurrentListener<X>
            Create<X>(
                Action<X> onValue,
                Action<Exception> onError,
                Action onCompleted,
                CancellationToken cancellationToken
            )
        {
            return
                new AnonymousCurrentListener<X>(
                    onValue,
                    onError,
                    onCompleted,
                    cancellationToken
                );
        }
    }
}
