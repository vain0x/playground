using System;
using System.Collections.Generic;

using Xunit;

namespace Structures
{
    public class VecTests
    {
        [Fact]
        public void BasicTests()
        {
            var vec = Vec.Of("a", "b", "c");

            vec.Length.Is(3);
            vec[0].Is("a");
            vec[1].Is("b");
            vec[2].Is("c");
        }

        [Fact]
        public void ItSupportsStructuralEquality()
        {
            Vec<int> F() => Vec.Of(1, 2, 3);

            var first = F();
            var second = F();

            first.Is(second);
            (first.Equals(second)).Is(true);
            (first == second).Is(true);
            (!ReferenceEquals(first, second)).Is(true);
        }

        [Fact]
        public void ItSupportsLexicographicalOrderByDefault()
        {
            (Vec.Of(1, 2, 3) < Vec.Of(1, 2, 4)).Is(true);
            (Vec.Of(1, 2, 4) < Vec.Of(1, 3)).Is(true);
        }

        public class ToStringTests
        {
            [Fact]
            public void ItCanPrintsEmpty()
            {
                Vec.Empty<object>().ToString()
                    .Is("[]");
            }

            [Fact]
            public void ItCanPrintsNonEmpty()
            {
                Vec.Of(1, 2, 3).ToString()
                    .Is("[1, 2, 3]");
            }
        }
    }
}
