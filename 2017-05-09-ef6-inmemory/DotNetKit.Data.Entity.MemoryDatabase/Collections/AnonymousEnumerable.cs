using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.Collections
{
    sealed class AnonymousEnumerable<TValue>
        : IEnumerable<TValue>
    {
        readonly Func<IEnumerator<TValue>> getEnumerator;

        IEnumerator<TValue> IEnumerable<TValue>.GetEnumerator()
        {
            return getEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return getEnumerator();
        }

        public AnonymousEnumerable(Func<IEnumerator<TValue>> getEnumerator)
        {
            this.getEnumerator = getEnumerator;
        }
    }
}
