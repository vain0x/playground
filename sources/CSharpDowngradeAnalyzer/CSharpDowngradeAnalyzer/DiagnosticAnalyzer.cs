using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using CSharpDowngradeAnalyzer.Analyzers;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace CSharpDowngradeAnalyzer
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class CSharpDowngradeAnalyzerAnalyzer
        : DiagnosticAnalyzer
    {
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics =>
            ImmutableArray.Create(PropertySyntaxAnalyzer.PropertyRule);

        public override void Initialize(AnalysisContext context)
        {
            PropertySyntaxAnalyzer.Analyzer.Initialize(context);
        }
    }
}
