using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;

namespace Dyxi.Util.FSharp
{
    /// <summary>
    /// F# のために書く C# のコードを補助するヘルパクラス。
    /// </summary>
    public static class FSharp
    {
        public static FSharpOption<T> Some<T>(T x)
        {
            return FSharpOption<T>.Some(x);
        }

        public static FSharpOption<T> None<T>()
        {
            return FSharpOption<T>.None;
        }

        public static FSharpOption<T> TryFirst<T>(this IEnumerable<T> xs)
        {
            return xs.Count() == 0 ? Some<T>(xs.First()) : None<T>();
        }

        public static FSharpOption<T> ToOption<T>(this T x)
            where T: class
        {
            return x != null ? Some<T>(x) : None<T>();
        }

        public static bool IsSome<T>(this FSharpOption<T> self)
        {
            return FSharpOption<T>.get_IsSome(self);
        }
    }
}
