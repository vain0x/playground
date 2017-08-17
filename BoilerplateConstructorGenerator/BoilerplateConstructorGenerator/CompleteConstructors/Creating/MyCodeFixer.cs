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
    public sealed class MyCodeFixer
    {
        SemanticModel SemanticModel { get; }

        TypeDeclarationSyntax TypeDecl { get; }

        ITypeSymbol TypeSymbol { get; }

        CancellationToken CancellationToken { get; }

        LanguageVersion LanguageVersion =>
            (TypeDecl.SyntaxTree.Options as CSharpParseOptions)?.LanguageVersion
            ?? LanguageVersion.CSharp6;

        public async Task<Document> FixAsync(Document document)
        {
            var tree = await document.GetSyntaxTreeAsync(CancellationToken).ConfigureAwait(false);
            var root = await tree.GetRootAsync(CancellationToken);

            var factory = new MySyntaxFactory(LanguageVersion);
            var varMembers = new VariableMemberCollector(SemanticModel).Collect(TypeDecl);

            var constructor =
                factory.CompleteConstructor(
                    SemanticModel,
                    TypeDecl,
                    varMembers
                );

            return
                document.WithSyntaxRoot(
                    root.ReplaceNode(
                        TypeDecl,
                        TypeDecl.AddMembers(constructor)
                    ));
        }

        public MyCodeFixer(
                SemanticModel semanticModel,
                TypeDeclarationSyntax typeDecl,
                ITypeSymbol typeSymbol,
                CancellationToken cancellationToken)
        {
            SemanticModel = semanticModel;
            TypeDecl = typeDecl;
            TypeSymbol = typeSymbol;
            CancellationToken = cancellationToken;
        }

        public static async Task Register(CodeFixContext context)
        {
            var diagnostic = context.Diagnostics.FirstOrDefault();
            if (diagnostic == null) return;

            var document = context.Document;
            var cancellationToken = context.CancellationToken;

            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            var diagnosticSpan = diagnostic.Location.SourceSpan;
            var typeDecl =
                root
                .FindToken(diagnosticSpan.Start)
                .Parent
                .AncestorsAndSelf()
                .OfType<TypeDeclarationSyntax>()
                .First();

            var semanticModel = await document.GetSemanticModelAsync(cancellationToken);

            var typeSymbol = semanticModel.GetDeclaredSymbol(typeDecl, cancellationToken);

            context.RegisterCodeFix(
                CodeAction.Create(
                    diagnostic.Descriptor.Title.ToString(),
                    ct => new MyCodeFixer(semanticModel, typeDecl, typeSymbol, ct).FixAsync(document),
                    equivalenceKey: diagnostic.Id
                ),
                diagnostic
            );
        }
    }
}
