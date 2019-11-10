using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TestHelper;

namespace RecordTypeAnalyzer.Test.Analyzers
{
    [TestClass]
    public sealed class RecordTypeGeneratorTest
        : ConventionCodeFixVerifier
    {
        [TestMethod]
        public void NoDiagnosticCase() => VerifyCSharpByConvention();

        [TestMethod]
        public void GenerateForClassCase() => VerifyCSharpByConvention();

        protected override DiagnosticAnalyzer GetCSharpDiagnosticAnalyzer() =>
            new RecordTypeAnalyzerAnalyzer();

        protected override CodeFixProvider GetCSharpCodeFixProvider() =>
            new RecordTypeAnalyzerCodeFixProvider();
    }
}
