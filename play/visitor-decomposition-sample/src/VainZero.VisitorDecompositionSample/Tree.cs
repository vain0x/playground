using System;

namespace VainZero.VisitorDecompositionSample
{
    public abstract class Tree
    {
        public abstract X Accept<X, Y>(TreeVisitor<X, Y> visitor, TreeDecomposer<X, Y> decomposer);
    }

    public class Leaf
        : Tree
    {
        public override X Accept<X, Y>(TreeVisitor<X, Y> visitor, TreeDecomposer<X, Y> decomposer)
        {
            return visitor.VisitLeaf();
        }
    }

    public class Node
        : Tree
    {
        public int Value { get; }
        public Tree Left { get; }
        public Tree Right { get; }

        public Node(int value, Tree left, Tree right)
        {
            Value = value;
            Left = left;
            Right = right;
        }

        public override X Accept<X, Y>(TreeVisitor<X, Y> visitor, TreeDecomposer<X, Y> decomposer)
        {
            return visitor.VisitNode(this, decomposer.Decompose(visitor, Left), decomposer.Decompose(visitor, Right));
        }
    }

    public abstract class TreeVisitor<TResult, TDecomposed>
    {
        public abstract TResult VisitLeaf();
        public abstract TResult VisitNode(Node node, TDecomposed left, TDecomposed right);
    }

    public interface TreeDecomposer<TResult, TDecomposed>
    {
        TDecomposed Decompose(TreeVisitor<TResult, TDecomposed> visitor, Tree tree);
    }
}
