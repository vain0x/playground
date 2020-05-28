using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.Collections
{
    sealed class StructuralEqualityComparer<T>
        : IEqualityComparer<T>
    {
        IEqualityComparer Comparer => StructuralComparisons.StructuralEqualityComparer;

        public bool Equals(T x, T y)
        {
            return Comparer.Equals(x, y);
        }

        public int GetHashCode(T obj)
        {
            return Comparer.GetHashCode(obj);
        }

        public static IEqualityComparer<T> Instance { get; } =
            new StructuralEqualityComparer<T>();
    }
}
