using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DotNetKit.Collections;

namespace DotNetKit.Sample
{
    static class MyLinq
    {
        // Your LINQ function.
        static X MyFirstCore<X, TEnumerator>(TEnumerator enumerator)
            where TEnumerator : struct, IEnumerator<X> // 'struct' constraint prevents boxing.
        {
            using (enumerator)
            {
                while (enumerator.MoveNext())
                {
                    return enumerator.Current;
                }
            }
            throw new Exception();
        }

        // This overload takes a list.
        // No allocations for enumeration.
        public static X MyFirst<X>(IReadOnlyList<X> list)
        {
            return MyFirstCore<X, ReadOnlyListEnumerator<X>>(new ReadOnlyListEnumerator<X>(list));
        }

        // This overload takes any enumerable.
        // You can DRY.
        // (IEnumerable<_>.GetEnumerator invokes boxing if the enumerator is a struct.)
        public static X MyFirst<X>(IEnumerable<X> enumerable)
        {
            return MyFirstCore<X, StructEnumerator<X>>(new StructEnumerator<X>(enumerable.GetEnumerator()));
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var xs = Enumerable.Range(0, 100).ToArray();
            var sum = 0L;
            MyLinq.MyFirst(xs);

            GC.Collect();
            var before = GC.GetTotalMemory(false);
            {
                for (var i = 0; i < 10000; i++)
                {
                    sum += MyLinq.MyFirst(xs);
                }
            }
            var after = GC.GetTotalMemory(false);
            var difference = after - before;

            // difference == 0

            Console.WriteLine("Memory addition: {0}", difference);
        }
    }
}
