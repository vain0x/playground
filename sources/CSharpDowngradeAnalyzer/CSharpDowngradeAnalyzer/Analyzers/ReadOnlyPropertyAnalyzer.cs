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
using Microsoft.CodeAnalysis.Formatting;

namespace CSharpDowngradeAnalyzer.Analyzers
{
    public static class ReadOnlyPropertyAnalyzer
    {
        public static DiagnosticDescriptor PropertyRule { get; } =
            new DiagnosticDescriptor(
                "CsdaProperty",
                "Property Declaration Syntax in C# 6",
                "Read-only auto properties, expression-bodied properties and properties with initializer aren't supported in C# 5",
                "Refactoring",
                DiagnosticSeverity.Warning,
                isEnabledByDefault: true
            );

        static SyntaxToken CreateIdentifier(string text)
        {
            if (SyntaxFacts.GetKeywordKind(text) != SyntaxKind.None
                || SyntaxFacts.GetContextualKeywordKind(text) != SyntaxKind.None
                )
            {
                return
                    SyntaxFactory.VerbatimIdentifier(
                        SyntaxFactory.TriviaList(),
                        text,
                        text,
                        SyntaxFactory.TriviaList()
                    );
            }

            return SyntaxFactory.Identifier(text);
        }

        static SyntaxToken FieldIdentifierFromPropertyName(string propertyName)
        {
            if (propertyName.Length > 0 && char.IsUpper(propertyName[0]))
            {
                var sb = new StringBuilder(propertyName);
                sb[0] = char.ToLowerInvariant(sb[0]);
                return CreateIdentifier(sb.ToString());
            }
            else
            {
                return CreateIdentifier("_" + propertyName);
            }
        }

        public sealed class Analyzer
        {
            TypeDeclarationSyntax TypeDecl { get; }

            SemanticModel SemanticModel { get; }

            public Analyzer(TypeDeclarationSyntax typeDecl, SemanticModel semanticModel)
            {
                TypeDecl = typeDecl;
                SemanticModel = semanticModel;
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

            public IEnumerable<PropertyDeclarationSyntax> FixablePropertyDecls()
            {
                foreach (var member in TypeDecl.Members)
                {
                    switch (member)
                    {
                        case PropertyDeclarationSyntax propertyDecl:
                            if (IsReadOnlyAuto(propertyDecl)
                                || propertyDecl.ExpressionBody != null
                                || propertyDecl.Initializer != null
                                )
                            {
                                yield return propertyDecl;
                            }
                            break;
                    }
                }
            }

            public void Analyze(SyntaxNodeAnalysisContext analysisContext)
            {
                foreach (var propertyDecl in FixablePropertyDecls())
                {
                    analysisContext.ReportDiagnostic(
                        Diagnostic.Create(PropertyRule, propertyDecl.GetLocation())
                    );
                }
            }

            static void AnalyzeTypeDecl(SyntaxNodeAnalysisContext analysisContext)
            {
                var typeDecl = (TypeDeclarationSyntax)analysisContext.Node;

                var options = typeDecl.SyntaxTree.Options as CSharpParseOptions;
                if (options == null) return;
                if (!options.LanguageVersion.IsCSharp5OrEarlier()) return;

                new Analyzer(typeDecl, analysisContext.SemanticModel).Analyze(analysisContext);
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

            TypeDeclarationSyntax TypeDecl =>
                (TypeDeclarationSyntax)PropertyDecl.Parent;

            IPropertySymbol PropertySymbol { get; }

            CancellationToken CancellationToken { get; }

            public Fixer(
                SyntaxNode root,
                PropertyDeclarationSyntax propertyDecl,
                IPropertySymbol propertySymbol,
                CancellationToken cancellationToken)
            {
                Root = root;
                PropertyDecl = propertyDecl;
                PropertySymbol = propertySymbol;
                CancellationToken = cancellationToken;
            }

            bool IsReadOnlyAuto(PropertyDeclarationSyntax propertyDecl, IPropertySymbol symbol)
            {
                if (propertyDecl.ExpressionBody != null) return false;

                var accessorList = propertyDecl.AccessorList;
                if (accessorList == null) return false;
                if (accessorList.Accessors.Any(a => a.Body != null)) return false;

                return
                    symbol.SetMethod == null
                    && symbol.GetMethod != null
                    && !symbol.IsAbstract;
            }

            static SyntaxTriviaList WhitespaceTriviaList()
            {
                return SyntaxFactory.TriviaList(SyntaxFactory.Whitespace(" "));
            }

            static SyntaxTriviaList EndOfLineTriviaList()
            {
                return SyntaxFactory.TriviaList(SyntaxFactory.EndOfLine(Environment.NewLine));
            }

            bool IsMultiline(SyntaxNode node)
            {
                return node.ToString().Trim().IndexOfAny(new[] { '\r', '\n' }) >= 0;
            }

            bool TryExpandExpressionBody(out SyntaxNode newRoot)
            {
                var expression = PropertyDecl.ExpressionBody?.Expression;
                if (expression != null)
                {
                    var isMultiline = IsMultiline(expression);

                    var returnStatement =
                        SyntaxFactory.ReturnStatement(
                            SyntaxFactory.Token(SyntaxKind.ReturnKeyword)
                            .WithTrailingTrivia(
                                isMultiline ? EndOfLineTriviaList() : WhitespaceTriviaList()
                            ),
                            expression,
                            SyntaxFactory.Token(SyntaxKind.SemicolonToken)
                        )
                        //.WithLeadingTrivia(WhitespaceTriviaList())
                        //.WithTrailingTrivia(WhitespaceTriviaList())
                        ;
                    var accessorList =
                        SyntaxFactory.AccessorList(
                            SyntaxFactory.List<AccessorDeclarationSyntax>().Add(
                                SyntaxFactory.AccessorDeclaration(
                                    SyntaxKind.GetAccessorDeclaration,
                                        SyntaxFactory.Block(returnStatement)
                                    )));
                    newRoot =
                        Root.ReplaceNode(
                            PropertyDecl,
                            PropertyDecl
                            .WithExpressionBody(null)
                            .WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.None))
                            .WithAccessorList(accessorList)
                            //.WithIdentifier(
                            //    PropertyDecl.Identifier
                            //    .WithTrailingTrivia(EndOfLineTriviaList())
                            //)
                            .WithAdditionalAnnotations(Formatter.Annotation)
                        );
                    return true;
                }

                newRoot = default(SyntaxNode);
                return false;
            }

            bool TryAddPrivateSetter(out SyntaxNode newRoot)
            {
                if (TypeDecl.IsKind(SyntaxKind.ClassDeclaration)
                    && PropertySymbol.DeclaredAccessibility != Accessibility.Private
                    && !PropertySymbol.IsOverride
                    && PropertyDecl.Initializer == null
                    && IsReadOnlyAuto(PropertyDecl, PropertySymbol)
                    )
                {
                    var accessor =
                        SyntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration)
                        .WithModifiers(
                            SyntaxFactory.TokenList(
                                SyntaxFactory.Token(SyntaxKind.PrivateKeyword)
                            ))
                        .WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken));
                    newRoot =
                        Root.ReplaceNode(
                            PropertyDecl,
                            PropertyDecl
                            .AddAccessorListAccessors(accessor)
                        );
                    return true;
                }

                newRoot = default(SyntaxNode);
                return false;
            }

            bool TryAddBackingField(out SyntaxNode newRoot)
            {
                if (IsReadOnlyAuto(PropertyDecl, PropertySymbol))
                {
                    var typeDecl = PropertyDecl.Parent as TypeDeclarationSyntax;
                    if (typeDecl != null)
                    {
                        var propertyDeclIndex = typeDecl.Members.IndexOf(PropertyDecl);
                        if (propertyDeclIndex >= 0)
                        {
                            var fieldIdentifier = FieldIdentifierFromPropertyName(PropertySymbol.Name);

                            var varDecl = SyntaxFactory.VariableDeclarator(fieldIdentifier);
                            if (PropertyDecl.Initializer != null)
                            {
                                varDecl = varDecl.WithInitializer(PropertyDecl.Initializer);
                            }

                            var fieldDecl =
                                SyntaxFactory.FieldDeclaration(
                                    SyntaxFactory.VariableDeclaration(
                                        PropertyDecl.Type,
                                        SyntaxFactory.SingletonSeparatedList(varDecl)
                                    ));
                            if (PropertySymbol.SetMethod == null)
                            {
                                fieldDecl =
                                    fieldDecl.WithModifiers(
                                        SyntaxFactory.TokenList(
                                            SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword)
                                        ));
                            }

                            var returnStatement =
                                SyntaxFactory.ReturnStatement(
                                    SyntaxFactory.Token(SyntaxKind.ReturnKeyword),
                                    SyntaxFactory.IdentifierName(fieldIdentifier),
                                    SyntaxFactory.Token(SyntaxKind.SemicolonToken)
                                );
                            var accessorList =
                                SyntaxFactory.AccessorList(
                                    SyntaxFactory.List<AccessorDeclarationSyntax>().Add(
                                        SyntaxFactory.AccessorDeclaration(
                                            SyntaxKind.GetAccessorDeclaration,
                                                SyntaxFactory.Block(returnStatement)
                                            )));

                            var members =
                                typeDecl.Members
                                .Replace(
                                    PropertyDecl,
                                    PropertyDecl
                                    .WithInitializer(null)
                                    .WithAccessorList(accessorList)
                                    .WithAdditionalAnnotations(Formatter.Annotation)
                                )
                                .Insert(
                                    propertyDeclIndex,
                                    fieldDecl
                                    .WithAdditionalAnnotations(Formatter.Annotation)
                                );

                            newRoot =
                                Root.ReplaceNode(
                                    typeDecl,
                                    typeDecl.WithMembers(members)
                                );
                            return true;
                        }
                    }
                }

                newRoot = default(SyntaxNode);
                return false;
            }

            SyntaxNode FixRoot()
            {
                var root = default(SyntaxNode);
                if (TryExpandExpressionBody(out root)) return root;
                if (TryAddPrivateSetter(out root)) return root;
                if (TryAddBackingField(out root)) return root;
                return Root;
            }

            Task<Document> FixAsync(Document document)
            {
                return Task.FromResult(document.WithSyntaxRoot(FixRoot()));
            }

            public static async Task RegisterCodeFixesAsync(CodeFixContext context)
            {
                var document = context.Document;

                var root = await document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
                if (root == null) return;

                var diagnostic = context.Diagnostics.First();
                var diagnosticSpan = diagnostic.Location.SourceSpan;

                var typeDecl = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<TypeDeclarationSyntax>().FirstOrDefault();
                if (typeDecl == null) return;

                if (!document.SupportsSemanticModel) return;
                var semanticModel = await document.GetSemanticModelAsync(context.CancellationToken);
                if (semanticModel == null) return;

                var analyzer = new Analyzer(typeDecl, semanticModel);

                foreach (var propertyDecl in analyzer.FixablePropertyDecls())
                {
                    var propertySymbol = semanticModel.GetDeclaredSymbol(propertyDecl);
                    if (propertySymbol == null) return;

                    context.RegisterCodeFix(
                        CodeAction.Create(
                            "Fix ",
                            ct => new Fixer(root, propertyDecl, propertySymbol, ct).FixAsync(document),
                            equivalenceKey: $"{PropertyRule.Id}_{propertySymbol.ToDisplayString()}"
                        ),
                        diagnostic
                    );
                }
            }
        }
    }
}
