using System;

namespace Structures
{
    internal sealed class AsyncCell<T>
    {
        T content;

        public T Value => content;

        public AsyncCell(T value)
        {
            content = value;
        }
    }
}
