using System;
using System.Linq;
using Xunit;

namespace Structures
{
    public sealed class JsonValue
        : UnionBase<JsonValue>
    {
        public sealed class String : Variant<String, string> { }
        public sealed class Number : Variant<Number, double> { }
        public sealed class Array : Variant<Array, JsonValue[]> { }

        public static readonly Matcher<JsonValue, Tys<String, Tys<Number, Tys<Array, Tys>>>> Function = ImplicitMatcher;
    }

    public class UnionTests
    {
        static readonly Func<JsonValue, string> TypeName =
            JsonValue.Function
            .With(s => "string")
            .With(n => "number")
            .With(a => "array");

        [Fact]
        public void Test1()
        {
            var stringValue = JsonValue.String.Create("hello");
            var numberValue = JsonValue.Number.Create(0);

            TypeName(stringValue).Is("string");
            TypeName(numberValue).Is("number");
        }
    }
}
