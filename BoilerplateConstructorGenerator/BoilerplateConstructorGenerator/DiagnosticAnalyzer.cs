using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace BoilerplateConstructorGenerator
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class BoilerplateConstructorGeneratorAnalyzer : DiagnosticAnalyzer
    {
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics =>
            DiagnosticProvider.SupportedDiagnostics;

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(AnalyzeTypeToGenerateCompleteConstructor, SyntaxKind.ClassDeclaration);
            context.RegisterSyntaxNodeAction(AnalyzeTypeToGenerateCompleteConstructor, SyntaxKind.StructDeclaration);
        }

        static void AnalyzeTypeToGenerateCompleteConstructor(SyntaxNodeAnalysisContext context)
        {
            var typeDecl = (TypeDeclarationSyntax)context.Node;
            new CompleteConstructorGeneration.MyAnalyzer(context, typeDecl).Analyze();
        }
    }
}
