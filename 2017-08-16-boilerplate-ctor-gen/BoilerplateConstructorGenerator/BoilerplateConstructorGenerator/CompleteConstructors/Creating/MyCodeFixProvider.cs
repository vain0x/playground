using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Rename;
using Microsoft.CodeAnalysis.Text;

namespace BoilerplateConstructorGenerator.CompleteConstructors.Creating
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(MyCodeFixProvider)), Shared]
    public class MyCodeFixProvider : CodeFixProvider
    {
        static readonly ImmutableArray<string> s_fixableDiagnosticIds =
            ImmutableArray.Create(DiagnosticProvider.CompleteConstructorGeneration.Id);

        public sealed override ImmutableArray<string> FixableDiagnosticIds =>
            s_fixableDiagnosticIds;

        public sealed override FixAllProvider GetFixAllProvider()
        {
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var diagnostic = context.Diagnostics.FirstOrDefault();
            if (diagnostic == null) return;

            var document = context.Document;
            var ct = context.CancellationToken;

            var root = await document.GetSyntaxRootAsync(ct).ConfigureAwait(false);
            if (root == null) return;

            var typeDecl =
                root
                .FindToken(diagnostic.Location.SourceSpan.Start)
                .Parent
                .AncestorsAndSelf()
                .OfType<TypeDeclarationSyntax>()
                .FirstOrDefault();
            if (typeDecl == null) return;

            var semanticModel = await document.GetSemanticModelAsync(ct);
            if (semanticModel == null) return;

            // Generate fixing method.

            var varMembers = new VariableMemberCollector(semanticModel).Collect(typeDecl);
            if (varMembers.All(m => m.HasInitializer)) return;

            var languageVersion =
                (typeDecl.SyntaxTree.Options as CSharpParseOptions)?.LanguageVersion
                ?? LanguageVersion.CSharp6;
            var factory = new MySyntaxFactory(languageVersion);

            async Task<Document> FixAsync()
            {
                var constructor = factory.CompleteConstructor(semanticModel, typeDecl, varMembers);
                return
                    document.WithSyntaxRoot(
                        root.ReplaceNode(
                            typeDecl,
                            typeDecl.AddMembers(constructor)
                        ));
            }

            context.RegisterCodeFix(
                CodeAction.Create(
                    diagnostic.Descriptor.Title.ToString(),
                    _ => FixAsync(),
                    equivalenceKey: diagnostic.Id
                ),
                diagnostic
            );
        }
    }
}
