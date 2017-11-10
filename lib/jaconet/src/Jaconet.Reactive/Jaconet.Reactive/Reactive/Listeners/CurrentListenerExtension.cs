using System;
using System.Collections.Generic;
using System.Text;

namespace Jaconet.Reactive.Listeners
{
    /// <summary>
    /// Provides extension methods.
    /// </summary>
    public static class CurrentListenerExtension
    {
        /// <summary>
        /// Creates a listener which boxes and forwards values.
        /// </summary>
        /// <typeparam name="X"></typeparam>
        /// <param name="this"></param>
        /// <returns></returns>
        public static CurrentListener<X> Box<X>(this CurrentListener @this)
        {
            return new BoxCurrentListener<X>(@this);
        }
    }
}
