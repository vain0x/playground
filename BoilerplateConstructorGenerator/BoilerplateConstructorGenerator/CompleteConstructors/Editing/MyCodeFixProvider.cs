using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
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

namespace BoilerplateConstructorGenerator.CompleteConstructors.Editing
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(MyCodeFixProvider)), Shared]
    public sealed class MyCodeFixProvider
        : CodeFixProvider
    {
        static readonly ImmutableArray<string> s_fixableDiagnosticIds =
            ImmutableArray.Create(DiagnosticProvider.CompleteConstructorFix.Id);

        public sealed override ImmutableArray<string> FixableDiagnosticIds =>
            s_fixableDiagnosticIds;

        public sealed override FixAllProvider GetFixAllProvider()
        {
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var document = context.Document;
            var ct = context.CancellationToken;

            var diagnostic = context.Diagnostics.FirstOrDefault();
            if (diagnostic == null) return;

            var diagnosticSpan = diagnostic.Location.SourceSpan;

            var root = await document.GetSyntaxRootAsync(ct).ConfigureAwait(false);
            if (root == null) return;

            var semanticModel = await document.GetSemanticModelAsync(ct);
            if (semanticModel == null) return;

            var typeDecl =
                root
                .FindToken(diagnosticSpan.Start)
                .Parent
                .AncestorsAndSelf()
                .OfType<TypeDeclarationSyntax>()
                .FirstOrDefault();
            if (typeDecl == null) return;

            var tree = typeDecl.SyntaxTree;

            var typeSymbol = semanticModel.GetDeclaredSymbol(typeDecl, ct);
            if (typeSymbol == null) return;

            var varMembers = new VariableMemberCollector(semanticModel).Collect(typeDecl);

            var languageVersion =
                (tree.Options as CSharpParseOptions)?.LanguageVersion
                ?? LanguageVersion.CSharp6;
            var factory = new MySyntaxFactory(languageVersion);
            factory.FixCompleteConstructor(semanticModel, typeDecl, varMembers, out var constructorDecl, out var fix);

            if (constructorDecl != null && fix != null)
            {
                context.RegisterCodeFix(
                    CodeAction.Create(
                        diagnostic.Descriptor.Title.ToString(),
                        async _ =>
                            document.WithSyntaxRoot(
                                root.ReplaceNode(
                                    constructorDecl,
                                    fix()
                                )),
                        equivalenceKey: diagnostic.Id
                    ),
                    diagnostic
                );
            }
        }
    }
}
