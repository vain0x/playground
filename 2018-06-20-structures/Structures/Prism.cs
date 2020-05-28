using System;
using System.Collections.Generic;
using System.Linq;

namespace Structures
{
    public interface IPrism<TSource, T>
    {
        Opt<T> Try(TSource source);
        TSource With(TSource source, T value);
    }


}
