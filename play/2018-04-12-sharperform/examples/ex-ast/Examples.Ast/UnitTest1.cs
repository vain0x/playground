using System;
using Xunit;

namespace Examples.Ast
{
    using Sharperform;

    [Derive("Immutable")]
    public sealed partial class Person
    {
        public string Name { get; }
        public int Age { get; }
    }
}

namespace Sharperform
{
    using System;

    [AttributeUsage(AttributeTargets.Class, Inherited = false, AllowMultiple = true)]
    internal sealed class DeriveAttribute
        : Attribute
    {
        public string Trait { get; }

        public DeriveAttribute(string trait)
        {
            Trait = trait;
        }
    }
}
