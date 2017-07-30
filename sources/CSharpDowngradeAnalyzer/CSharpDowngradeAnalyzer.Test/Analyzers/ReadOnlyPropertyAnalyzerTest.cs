using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TestHelper;

namespace CSharpDowngradeAnalyzer.Test.Analyzers
{
    [TestClass]
    public sealed class ReadOnlyPropertyAnalyzerTest
        : ConventionCodeFixVerifier
    {
        [TestMethod]
        public void NonvirtualCase() => VerifyCSharpByConvention();

        protected override DiagnosticAnalyzer GetCSharpDiagnosticAnalyzer()
        {
            return new CSharpDowngradeAnalyzerAnalyzer();
        }

        protected override CodeFixProvider GetCSharpCodeFixProvider()
        {
            return new CSharpDowngradeAnalyzerCodeFixProvider();
        }
    }
}
