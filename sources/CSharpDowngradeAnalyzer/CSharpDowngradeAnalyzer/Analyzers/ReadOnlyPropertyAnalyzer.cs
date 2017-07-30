using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Rename;
using Microsoft.CodeAnalysis.Text;

namespace CSharpDowngradeAnalyzer.Analyzers
{
    public static class ReadOnlyPropertyAnalyzer
    {
        public static DiagnosticDescriptor Rule { get; } =
            new DiagnosticDescriptor(
                "CsdaRop",
                "Read-only Property",
                "Read-only propety isn't supported in C# 5",
                "Refactoring",
                DiagnosticSeverity.Warning,
                isEnabledByDefault: true
            );

        public sealed class Analyzer
        {
            SyntaxNodeAnalysisContext AnalysisContext { get; }

            SemanticModel SemanticModel => AnalysisContext.SemanticModel;

            TypeDeclarationSyntax TypeDecl { get; }

            public Analyzer(SyntaxNodeAnalysisContext analysisContext,
                        TypeDeclarationSyntax typeDecl)
            {
                AnalysisContext = analysisContext;
                TypeDecl = typeDecl;
            }

            bool IsReadOnlyAuto(PropertyDeclarationSyntax propertyDecl)
            {
                if (propertyDecl.ExpressionBody != null) return false;

                var accessorList = propertyDecl.AccessorList;
                if (accessorList == null) return false;
                if (accessorList.Accessors.Any(a => a.Body != null)) return false;

                var symbol = SemanticModel.GetDeclaredSymbol(propertyDecl);
                if (symbol == null) return false;

                return
                    symbol.SetMethod == null
                    && symbol.GetMethod != null
                    && !symbol.IsAbstract;
            }

            public void Analyze()
            {
                foreach (var member in TypeDecl.Members)
                {
                    switch (member)
                    {
                        case PropertyDeclarationSyntax propertyDecl:
                            if (IsReadOnlyAuto(propertyDecl))
                            {
                                AnalysisContext.ReportDiagnostic(
                                    Diagnostic.Create(Rule, propertyDecl.GetLocation())
                                );
                            }
                            break;
                    }
                }
            }

            static void AnalyzeTypeDecl(SyntaxNodeAnalysisContext context)
            {
                var options = context.SemanticModel.SyntaxTree.Options as CSharpParseOptions;
                if (options == null) return;
                if (!options.LanguageVersion.IsCSharp5OrEarlier()) return;

                var typeDecl = (TypeDeclarationSyntax)context.Node;
                new Analyzer(context, typeDecl).Analyze();
            }

            public static void Initialize(AnalysisContext context)
            {
                context.RegisterSyntaxNodeAction(AnalyzeTypeDecl, SyntaxKind.ClassDeclaration);
                context.RegisterSyntaxNodeAction(AnalyzeTypeDecl, SyntaxKind.StructDeclaration);
            }
        }

        public sealed class Fixer
        {
            SyntaxNode Root { get; }

            PropertyDeclarationSyntax PropertyDecl { get; }

            CancellationToken CancellationToken { get; }

            public Fixer(
                SyntaxNode root,
                PropertyDeclarationSyntax propertyDecl,
                CancellationToken cancellationToken)
            {
                Root = root;
                PropertyDecl = propertyDecl;
                CancellationToken = cancellationToken;
            }

            Task<Document> FixAsync(Document document)
            {
                var accessor =
                    SyntaxFactory.AccessorDeclaration(SyntaxKind.SetKeyword)
                    .WithModifiers(
                        SyntaxFactory.TokenList(
                            SyntaxFactory.Token(SyntaxKind.PrivateKeyword)
                        ));
                return
                    Task.FromResult(
                        document.WithSyntaxRoot(
                            Root.ReplaceNode(
                                PropertyDecl,
                                PropertyDecl.AddAccessorListAccessors(accessor)
                            )));
            }

            public static async Task RegisterCodeFixesAsync(CodeFixContext context)
            {
                var document = context.Document;

                var root = await document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
                if (root == null) return;

                var diagnostic = context.Diagnostics.First();
                var diagnosticSpan = diagnostic.Location.SourceSpan;

                var propertyDecl = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<PropertyDeclarationSyntax>().FirstOrDefault();
                if (propertyDecl == null) return;

                if (!document.SupportsSemanticModel) return;
                var semanticModel = await document.GetSemanticModelAsync(context.CancellationToken);
                if (semanticModel == null) return;

                context.RegisterCodeFix(
                    CodeAction.Create(
                        Rule.Title.ToString(),
                        ct => new Fixer(root, propertyDecl, ct).FixAsync(document),
                        equivalenceKey: Rule.Id
                    ),
                    diagnostic
                );
            }
        }
    }
}
