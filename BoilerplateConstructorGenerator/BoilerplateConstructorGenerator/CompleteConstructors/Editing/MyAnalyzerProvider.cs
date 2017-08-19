using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace BoilerplateConstructorGenerator.CompleteConstructors.Editing
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class MyAnalyzerProvider
        : DiagnosticAnalyzer
    {
        static readonly ImmutableArray<DiagnosticDescriptor> s_suppertedDiagnostics =
            ImmutableArray.Create(DiagnosticProvider.CompleteConstructorFix);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics =>
            s_suppertedDiagnostics;

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(Analyze, SyntaxKind.ClassDeclaration);
            context.RegisterSyntaxNodeAction(Analyze, SyntaxKind.StructDeclaration);
        }

        static void Analyze(SyntaxNodeAnalysisContext context)
        {
            var typeDecl = (TypeDeclarationSyntax)context.Node;
            var semanticModel = context.SemanticModel;

            var typeSymbol = semanticModel.GetDeclaredSymbol(typeDecl);
            if (typeSymbol == null || typeSymbol.IsAbstract || typeSymbol.IsStatic) return;

            var varMembers = new VariableMemberCollector(semanticModel).Collect(typeDecl);
            if (varMembers.All(v => v.HasInitializer)) return;

            var languageVersion =
                (typeDecl.SyntaxTree.Options as CSharpParseOptions)?.LanguageVersion
                ?? LanguageVersion.CSharp6;
            var factory = new MySyntaxFactory(languageVersion);
            factory.FixCompleteConstructor(semanticModel, typeDecl, varMembers, out var constructorDecl, out var fix);

            if (
                constructorDecl != null
                && fix != null
                && constructorDecl.Identifier != null
                && typeDecl.Identifier != null
                )
            {
                context.ReportDiagnostic(
                    Diagnostic.Create(
                        DiagnosticProvider.CompleteConstructorFix,
                        constructorDecl.Identifier.GetLocation(),
                        typeDecl.Identifier.Text
                    ));
            }
        }
    }
}
