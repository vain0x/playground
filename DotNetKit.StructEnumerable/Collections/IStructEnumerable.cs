using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.Collections
{
    /// <summary>
    /// Represents a sequence.
    /// Equivalent to <see cref="IEnumerable{T}"/>
    /// except the enumerator type is specified and must be a value type.
    /// <para lang="ja">
    /// <see cref="IEnumerable{T}"/> と同じく、シーケンスを表す。
    /// ただし、生成する列挙子の型は特定の値型でなければならない。
    /// </para>
    /// </summary>
    /// <typeparam name="TValue"></typeparam>
    /// <typeparam name="TEnumerator"></typeparam>
    public interface IStructEnumerable<out TValue, out TEnumerator>
        : IEnumerable<TValue>
        where TEnumerator : struct, IEnumerator<TValue>
    {
        /// <summary>
        /// Gets a struct enumerator.
        /// </summary>
        /// <returns></returns>
        new TEnumerator GetEnumerator();
    }
}
