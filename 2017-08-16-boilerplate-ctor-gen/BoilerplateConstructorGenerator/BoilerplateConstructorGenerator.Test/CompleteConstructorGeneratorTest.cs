﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TestHelper;

namespace BoilerplateConstructorGenerator.Test
{
    [TestClass]
    public sealed class CompleteConstructorGeneratorTest
        : ConventionCodeFixVerifier
    {
        protected override DiagnosticAnalyzer GetCSharpDiagnosticAnalyzer()
        {
            return new CompleteConstructors.Creating.MyAnalyzerProvider();
        }

        protected override CodeFixProvider GetCSharpCodeFixProvider()
        {
            return new CompleteConstructors.Creating.MyCodeFixProvider();
        }

        [TestMethod]
        public void GenerationCase() => VerifyCSharpByConvention();

        [TestMethod]
        public void NoReportCase() => VerifyCSharpByConvention();
    }
}
