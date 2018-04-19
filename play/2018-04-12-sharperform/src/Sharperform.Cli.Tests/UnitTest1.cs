using System;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Xunit;
using Xunit.Abstractions;

namespace Sharperform.Cli.Tests
{
    public sealed class UnitTest
    {
        [Fact]
        public void Ok()
        {
            Assert.Equal(3, 1 + 2);
        }
    }

    public sealed class CompileExampleAst
    {
        ITestOutputHelper Output { get; }

        public CompileExampleAst(ITestOutputHelper output)
        {
            Output = output;
        }
    }
}
