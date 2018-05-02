using System;
using System.Collections.Generic;
using System.Text;
using Xunit;
using static VainZero.Parsing.ParserPresetModule;

namespace VainZero.Parsing
{
    public sealed class ParserPresetModuleTest
    {
        [Fact]
        public void Test_Int64Parser()
        {
            var p = Int64Parser;

            Assert.Equal(("rest", 0L), p.Parse("0rest").OkTuple);
            Assert.Equal(("", 112358132134L), p.Parse("112358132134").OkTuple);

            Assert.False(p.Parse("").IsOk);
            Assert.False(p.Parse("abc").IsOk);
        }

        [Fact]
        public void Test_IdentifierCharParser()
        {
            var p = IdentifierCharParser;

            Assert.Equal(("bc", 'a'), p.Parse("abc").OkTuple);
            Assert.Equal(("", '1'), p.Parse("1").OkTuple);

            Assert.False(p.Parse("#aa").IsOk);
        }

        [Fact]
        public void Test_IdentifierParser()
        {
            var p = IdentifierParser;

            Assert.Equal((" rest", "abc"), p.Parse("abc rest").OkTuple);
            Assert.Equal(("", "az0AZ9"), p.Parse("az0AZ9").OkTuple);

            Assert.False(p.Parse("").IsOk);
            Assert.False(p.Parse("1").IsOk);
            Assert.False(p.Parse("&a").IsOk);
        }
    }
}
