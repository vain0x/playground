using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;
using DotNetKit.Collections;

namespace DotNetKit.UnitTest
{
    public class ReadOnlyListEnumeratorTest
    {
        [Fact]
        public void test_MoveNext_and_Current()
        {
            var n = 5;

            var xs = Enumerable.Range(0, n).ToArray();
            var it = new ReadOnlyListEnumerator<int>(xs);

            var i = 0;
            while (it.MoveNext())
            {
                Assert.Equal(i, it.Current);
                i++;
            }

            Assert.Equal(n, i);
        }

        [Fact]
        public void test_MoveNext_throws_if_list_was_modified()
        {
            var n = 5;
            var xs = Enumerable.Range(0, n).ToList();
            var it = new ReadOnlyListEnumerator<int>(xs);

            xs.Add(n);

            foreach (var i in Enumerable.Range(0, n))
            {
                Assert.True(it.MoveNext());
            }

            Assert.Throws<InvalidOperationException>(() => it.MoveNext());
        }
    }
}
