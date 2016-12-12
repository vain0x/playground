using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.Collections
{
    /// <summary>
    /// Represents an enumerator which enumerates a readonly list.
    /// </summary>
    /// <typeparam name="TValue"></typeparam>
    public struct ReadOnlyListEnumerator<TValue>
        : IEnumerator<TValue>
    {
        readonly IReadOnlyList<TValue> list;
        readonly int count;

        int index;

        /// <summary>
        /// Gets the current value.
        /// </summary>
        public TValue Current
        {
            get { return list[index]; }
        }

        object IEnumerator.Current
        {
            get { return Current; }
        }

        /// <summary>
        /// Advances this.
        /// </summary>
        /// <returns></returns>
        public bool MoveNext()
        {
            ++index;

            if (index < count)
            {
                return true;
            }

            if (count != list.Count)
            {
                throw new InvalidOperationException("List was modified; enumeration may not execute.");
            }

            return false;
        }

        void IEnumerator.Reset()
        {
            throw new NotSupportedException();
        }

        void IDisposable.Dispose()
        {
        }

        /// <summary>
        /// Constructs an instance.
        /// </summary>
        /// <param name="list"></param>
        public ReadOnlyListEnumerator(IReadOnlyList<TValue> list)
        {
            this.list = list;
            count = list.Count;
            index = -1;
        }
    }
}
