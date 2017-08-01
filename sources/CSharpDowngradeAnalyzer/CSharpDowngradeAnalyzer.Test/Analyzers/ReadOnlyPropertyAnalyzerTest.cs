using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TestHelper;

namespace CSharpDowngradeAnalyzer.Test.Analyzers
{
    [TestClass]
    public sealed class ReadOnlyPropertyAnalyzerTest
        : ConventionCodeFixVerifier
    {
        CSharpParseOptions CSharp5 { get; } =
            new CSharpParseOptions(LanguageVersion.CSharp5);

        [TestMethod]
        public void AutoPropertyCase() => VerifyCSharpByConvention(parseOptions: CSharp5);

        [TestMethod]
        public void ExpressionBodiedCase() => VerifyCSharpByConvention(parseOptions: CSharp5);

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
