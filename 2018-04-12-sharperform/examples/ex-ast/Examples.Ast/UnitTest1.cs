using System;
using Sharperform;
using Xunit;

namespace Sharperform
{
    [DuplicateWithSuffix("A")]
    public class Foo
    {
    }


    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, Inherited = false, AllowMultiple = true)]
    public class DeriveAttribute
        : Attribute
    {
        public string Trait { get; }

        public DeriveAttribute(string trait)
        {
            Trait = trait;
        }
    }
}

namespace Examples.Ast
{
    /// <summary>
    /// Represents a person.
    /// </summary>
    [Derive("Immutable")]
    public sealed partial class Person
    {
        public string Name { get; }
        public int Age { get; }
    }

    [Derive("Immutable")]
    public partial struct Maybe<T>
    {
        public bool HasValue { get; }
        public T Value { get; }
    }
}

namespace Examples
{
    using Examples.Ast;

    public sealed class UnitTests
    {
        [Fact]
        public void HelloTest()
        {
            Assert.Equal(3, 1 + 2);
        }

        [Fact]
        public void MeybeTest()
        {
            var maybe = new Maybe<int>(true, 1);
            Assert.Equal(true, maybe.HasValue);
            Assert.Equal(1, maybe.Value);
        }
    }
}
