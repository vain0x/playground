using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.Simplification;

namespace BoilerplateConstructorGenerator.CompleteConstructors.Creating
{
    public sealed class MyAnalyzer
    {
        SyntaxNodeAnalysisContext AnalysisContext { get; }

        SemanticModel SemanticModel => AnalysisContext.SemanticModel;

        CancellationToken CancellationToken => AnalysisContext.CancellationToken;

        TypeDeclarationSyntax TypeDecl { get; }

        public MyAnalyzer(SyntaxNodeAnalysisContext analysisContext, TypeDeclarationSyntax typeDecl)
        {
            AnalysisContext = analysisContext;
            TypeDecl = typeDecl;
        }

        public void Analyze()
        {
            var typeSymbol = SemanticModel.GetDeclaredSymbol(TypeDecl);
            if (typeSymbol == null) return;

            if (typeSymbol.IsAbstract || typeSymbol.IsStatic) return;

            var varMembers = new VariableMemberCollector(SemanticModel).Collect(TypeDecl);

            if (varMembers.All(v => v.HasInitializer)) return;

            AnalysisContext.ReportDiagnostic(
                Diagnostic.Create(
                    DiagnosticProvider.CompleteConstructorGeneration,
                    TypeDecl.GetLocation(),
                    TypeDecl.Identifier.Text
                ));
        }
    }
}
