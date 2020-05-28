using System;

namespace Structures
{
    /// <summary>
    /// Wraps a value.
    ///
    /// Use this to share a struct.
    /// </summary>
    public sealed class Box<T>
    {
        public T Value { get; }
    }
}
