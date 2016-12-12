using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.Collections
{
    /// <summary>
    /// A struct enumerator which wraps an enumerator.
    /// </summary>
    /// <typeparam name="TValue"></typeparam>
    public struct StructEnumerator<TValue>
        : IEnumerator<TValue>
    {
        readonly IEnumerator<TValue> enumerator;

        /// <summary>
        /// Gets the current value of the underlying enumerator.
        /// </summary>
        public TValue Current
        {
            get
            {
                return enumerator.Current;
            }
        }

        object IEnumerator.Current
        {
            get
            {
                return ((IEnumerator)enumerator).Current;
            }
        }

        /// <summary>
        /// Advances the underlying enumerator.
        /// </summary>
        /// <returns></returns>
        public bool MoveNext()
        {
            return enumerator.MoveNext();
        }

        /// <summary>
        /// Resets the underlying enumerator.
        /// </summary>
        public void Reset()
        {
            enumerator.Reset();
        }

        /// <summary>
        /// Disposes all resources the underlying enumerator owns.
        /// </summary>
        public void Dispose()
        {
            enumerator.Dispose();
        }

        /// <summary>
        /// Constructs an instance.
        /// </summary>
        /// <param name="enumerator"></param>
        public StructEnumerator(IEnumerator<TValue> enumerator)
        {
            this.enumerator = enumerator;
        }
    }
}
