using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace BoilerplateConstructorGenerator.CompleteConstructors.Creating
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class MyAnalyzerProvider
        : DiagnosticAnalyzer
    {
        static readonly ImmutableArray<DiagnosticDescriptor> s_suppertedDiagnostics =
            ImmutableArray.Create(DiagnosticProvider.CompleteConstructorGeneration);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics =>
            s_suppertedDiagnostics;

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(AnalyzeTypeToGenerateCompleteConstructor, SyntaxKind.ClassDeclaration);
            context.RegisterSyntaxNodeAction(AnalyzeTypeToGenerateCompleteConstructor, SyntaxKind.StructDeclaration);
        }

        static void AnalyzeTypeToGenerateCompleteConstructor(SyntaxNodeAnalysisContext context)
        {
            var typeDecl = (TypeDeclarationSyntax)context.Node;
            var semanticModel = context.SemanticModel;

            if (typeDecl.Identifier == null) return;

            var typeSymbol = semanticModel.GetDeclaredSymbol(typeDecl);
            if (typeSymbol == null || typeSymbol.IsAbstract || typeSymbol.IsStatic) return;

            if (typeDecl.Members.Any(m => m.IsKind(SyntaxKind.ConstructorDeclaration))) return;

            context.ReportDiagnostic(
                Diagnostic.Create(
                    DiagnosticProvider.CompleteConstructorGeneration,
                    typeDecl.GetLocation(),
                    typeDecl.Identifier.Text
                ));
        }
    }
}
