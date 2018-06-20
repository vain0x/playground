using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using HashCode = System.Int32;

namespace Structures
{
    internal static class AvlTree<TValue>
    {
        internal interface IEntryModifier
        {
            TValue Create(out bool mutated);
            TValue Update(TValue source, out bool replaced, out bool mutated);
        }

        sealed class RemoveEntryModifier
            : IEntryModifier
        {
            public static RemoveEntryModifier Instance { get; } =
                new RemoveEntryModifier();

            public TValue Create(out bool created)
            {
                created = false;
                return default(TValue);
            }

            public TValue Update(TValue source, out bool replaced, out bool mutated)
            {
                mutated = true;
                replaced = false;
                return default(TValue);
            }
        }

        internal sealed class Node
        {
            public HashCode Key { get; }
            public TValue Value { get; }
            public Node Left { get; }
            public Node Right { get; }
            public int Height { get; }

            public Node(
                HashCode key,
                TValue value,
                Node left,
                Node right
            )
            {
                Key = key;
                Value = value;
                Left = left;
                Right = right;
                Height = 1 + Math.Max(left.Height, right.Height);
            }

            Node()
            {
                Key = default(HashCode);
                Value = default(TValue);
                Left = this;
                Right = this;
                Height = -1;
            }

            public static Node Empty { get; } = new Node();

            public bool IsEmpty => Height < 0;

            Node Mutate(Node left = null, Node right = null)
            {
                left = left ?? Left;
                right = right ?? Right;
                return left == Left && right == Right
                    ? this
                    : new Node(Key, Value, left, right);
            }

            #region Balance

            Node RotateLeft() => RotateLeft(this);
            Node RotateRight() => RotateRight(this);
            Node MakeBalanced() => MakeBalanced(this);

            // RotateLeft
            // from:
            //         T
            //        / \
            //       L   R
            //          / \
            //         X   Y
            // to:
            //        [R]
            //        / \
            //      [T]  Y
            //      / \
            //     L   X

            // RotateRight:
            // from:
            //         T
            //        / \
            //       L   R
            //      / \
            //     X   Y
            // to:
            //        [L]
            //        / \
            //       X  [T]
            //          / \
            //         Y   R

            static Node RotateLeft(Node tree)
            {
                Debug.Assert(!tree.IsEmpty);
                if (tree.Right.IsEmpty) return tree;

                var right = tree.Right;
                return right.Mutate(left: tree.Mutate(right: right.Left));
            }

            static Node RotateRight(Node tree)
            {
                Debug.Assert(!tree.IsEmpty);
                if (tree.Left.IsEmpty) return tree;

                var left = tree.Left;
                return left.Mutate(right: tree.Mutate(left: left.Right));
            }

            static Node DoubleLeft(Node tree)
            {
                Debug.Assert(!tree.IsEmpty);
                if (tree.Right.IsEmpty) return tree;

                return tree.Mutate(right: tree.Right.RotateRight()).RotateLeft();
            }

            static Node DoubleRight(Node tree)
            {
                Debug.Assert(!tree.IsEmpty);
                if (tree.Left.IsEmpty) return tree;

                return tree.Mutate(left: tree.Left.RotateLeft()).RotateRight();
            }

            static int Balance(Node tree) =>
                tree.Right.Height - tree.Left.Height;

            static Node MakeBalanced(Node tree)
            {
                if (Balance(tree) >= 2)
                    return Balance(tree.Right) < 0 ? DoubleLeft(tree) : RotateLeft(tree);
                if (Balance(tree) <= -2)
                    return Balance(tree.Left) > 0 ? DoubleRight(tree) : RotateRight(tree);
                return tree;
            }

            #endregion

            Node Min()
            {
                Debug.Assert(!IsEmpty);
                return Left.IsEmpty ? this : Left.Min();
            }

            public bool TryFind(HashCode key, out TValue value)
            {
                if (IsEmpty)
                {
                    value = default(TValue);
                    return false;
                }

                if (key == Key)
                {
                    value = Value;
                    return true;
                }

                return key < Key
                    ? Left.TryFind(key, out value)
                    : Right.TryFind(key, out value);
            }

            Node ReplaceRoot(TValue newValue)
            {
                Debug.Assert(!IsEmpty);

                return new Node(Key, newValue, Left, Right);
            }

            Node RemoveRoot()
            {
                Debug.Assert(!IsEmpty);

                if (Left.IsEmpty || Right.IsEmpty)
                    return Right.IsEmpty ? Left : Right;

                // これの次に大きいキーを持つノード (右部分木の左端) を新しい根にする。

                var next = Right.Min();
                return next.Mutate(Left, Right.Remove(next.Key)).MakeBalanced();
            }

            Node Remove(HashCode removeKey)
            {
                bool _discarded;
                return ModifyEntry(removeKey, RemoveEntryModifier.Instance, out _discarded);
            }

            public Node ModifyEntry<E>(HashCode key, E e, out bool mutated)
                where E : IEntryModifier
            {
                if (IsEmpty)
                {
                    var newValue = e.Create(out mutated);
                    if (!mutated) return this;
                    return new Node(key, newValue, Empty, Empty);
                }
                else if (key == Key)
                {
                    bool replaced;
                    var newValue = e.Update(Value, out replaced, out mutated);
                    if (!mutated) return this;
                    return replaced ? ReplaceRoot(newValue) : RemoveRoot();
                }
                else if (key < Key)
                {
                    var left = Left.ModifyEntry<E>(key, e, out mutated);
                    if (!mutated) return this;
                    return Mutate(left: left).MakeBalanced();
                }
                else
                {
                    var right = Right.ModifyEntry<E>(key, e, out mutated);
                    if (!mutated) return this;
                    return Mutate(right: right).MakeBalanced();
                }
            }

            public IEnumerable<TValue> ToEnumerable()
            {
                if (IsEmpty)
                    yield break;

                foreach (var item in Left.ToEnumerable())
                    yield return item;

                yield return Value;

                foreach (var item in Right.ToEnumerable())
                    yield return item;
            }
        }
    }
}
