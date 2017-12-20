using System;
using System.Collections.Generic;
using System.Text;

namespace Jaconet.Reactive.Currents
{
    /// <summary>
    /// Provides extension methods
    /// </summary>
    public static class CurrentExtension
    {
        /// <summary>
        /// Creates a current which produces mapped values.
        /// </summary>
        /// <typeparam name="X"></typeparam>
        /// <typeparam name="Y"></typeparam>
        /// <param name="this"></param>
        /// <param name="func"></param>
        /// <returns></returns>
        public static Current<Y> Map<X, Y>(this Current<X> @this, Func<X, Y> func)
        {
            return new MapCurrent<X, Y>(@this, func);
        }
    }
}
