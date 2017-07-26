using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using RecordTypeAnalyzer.Analyzers;

namespace RecordTypeAnalyzer
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class RecordTypeAnalyzerAnalyzer : DiagnosticAnalyzer
    {
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics =>
            DiagnosticProvider.SupportedDiagnostics;

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(AnalyzeTypeToGenerate, SyntaxKind.ClassDeclaration);
            context.RegisterSyntaxNodeAction(AnalyzeTypeToGenerate, SyntaxKind.StructDeclaration);
        }

        static void AnalyzeTypeToGenerate(SyntaxNodeAnalysisContext context)
        {
            new RecordTypeGeneratorAnalyzer(context, (TypeDeclarationSyntax)context.Node).Analyze();
        }
    }
}
