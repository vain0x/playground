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
