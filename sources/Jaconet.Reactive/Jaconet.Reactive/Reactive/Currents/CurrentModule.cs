using System;
using System.Collections.Generic;
using System.Text;

namespace Jaconet.Reactive.Currents
{
    /// <summary>
    /// Provides static methods.
    /// </summary>
    public static class CurrentModule
    {
        /// <summary>
        /// Creates a current,
        /// which enumerates the enumerable and produces each value when connected.
        /// </summary>
        /// <typeparam name="X"></typeparam>
        /// <param name="this"></param>
        /// <returns></returns>
        public static Current<X> FromEnumerable<X>(this IEnumerable<X> @this)
        {
            return new FromEnumerableCurrent<X>(@this);
        }
    }
}
