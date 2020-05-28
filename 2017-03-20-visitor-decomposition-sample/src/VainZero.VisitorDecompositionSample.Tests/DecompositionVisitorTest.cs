using System;
using Xunit;

namespace VainZero.VisitorDecompositionSample.Test
{
    public class DecompositionVisitorTest
    {
        static Leaf Leaf { get; } =
            new Leaf();

        public Tree SampleTree { get; } =
            new Node(1,
                new Node(2, Leaf, Leaf),
                new Node(3, Leaf, Leaf));

        public string Expected { get; } =
            @"(Node 1
  (Node 2
    leaf
    leaf
  )
  (Node 3
    leaf
    leaf
  )
)";

        [Fact]
        public void DocumentFromTree1Test()
        {
            var document = DocumentFromTree1.Run(SampleTree);
            Assert.Equal(Expected, document.Layout());
        }

        [Fact]
        public void DocumentFromTree2Test()
        {
            var document = DocumentFromTree2.Run(SampleTree);
            Assert.Equal(Expected, document.Layout());
        }
    }
}
