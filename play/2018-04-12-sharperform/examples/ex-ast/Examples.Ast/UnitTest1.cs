using System;
using Sharperform;
using Xunit;

namespace Sharperform
{
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
