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
                            if (IsReadOnlyAuto(propertyDecl)
                                || propertyDecl.ExpressionBody != null
                                || propertyDecl.Initializer != null
                                )
                            {
                                AnalysisContext.ReportDiagnostic(
                                    Diagnostic.Create(PropertyRule, propertyDecl.GetLocation())
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
                return
                    SyntaxFactory.TriviaList(
                        SyntaxFactory.SyntaxTrivia(SyntaxKind.WhitespaceTrivia, " ")
                    );
            }

            static SyntaxTriviaList EndOfLineTriviaList()
            {
                return
                    SyntaxFactory.TriviaList(
                        SyntaxFactory.SyntaxTrivia(SyntaxKind.EndOfLineTrivia, Environment.NewLine)
                    );
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
                        SyntaxFactory.ReturnStatement(expression)
                        .WithReturnKeyword(
                            SyntaxFactory.Token(SyntaxKind.ReturnKeyword)
                            .WithTrailingTrivia(
                                isMultiline ? EndOfLineTriviaList() : WhitespaceTriviaList()
                            ))
                        .WithSemicolonToken(
                            SyntaxFactory.Token(SyntaxKind.SemicolonToken)
                        )
                        .WithLeadingTrivia(WhitespaceTriviaList())
                        .WithTrailingTrivia(WhitespaceTriviaList());
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
                            .WithAccessorList(accessorList)
                            .WithIdentifier(
                                PropertyDecl.Identifier
                                .WithTrailingTrivia(EndOfLineTriviaList())
                            ));
                    return true;
                }

                newRoot = default(SyntaxNode);
                return false;
            }

            bool TryAddPrivateSetter(out SyntaxNode newRoot)
            {
                if (PropertySymbol.DeclaredAccessibility != Accessibility.Private
                    && IsReadOnlyAuto(PropertyDecl, PropertySymbol))
                {
                    var accessor =
                        SyntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration)
                        .WithModifiers(
                            SyntaxFactory.TokenList(
                                SyntaxFactory.Token(SyntaxKind.PrivateKeyword)
                            ))
                        .WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken));
                    newRoot =
                        Root.ReplaceNode(PropertyDecl, PropertyDecl.AddAccessorListAccessors(accessor));
                    return true;
                }

                newRoot = default(SyntaxNode);
                return false;
            }

            bool TryAddBackingField(out SyntaxNode newRoot)
            {
                if (IsReadOnlyAuto(PropertyDecl, PropertySymbol))
                {
                    var initializer = PropertyDecl.Initializer;
                    var typeDecl = PropertyDecl.Parent as TypeDeclarationSyntax;
                    if (typeDecl != null)
                    {
                        var propertyDeclIndex = typeDecl.Members.IndexOf(PropertyDecl);
                        if (propertyDeclIndex >= 0)
                        {
                            var varDecl =
                                SyntaxFactory.VariableDeclarator(
                                    FieldIdentifierFromPropertyName(PropertySymbol.Name)
                                );
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

                            newRoot =
                                Root.ReplaceNode(
                                    typeDecl,
                                    typeDecl
                                    .WithMembers(
                                        typeDecl.Members.Insert(
                                            propertyDeclIndex,
                                            fieldDecl
                                        ))
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

                var propertyDecl = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<PropertyDeclarationSyntax>().FirstOrDefault();
                if (propertyDecl == null) return;

                if (!document.SupportsSemanticModel) return;
                var semanticModel = await document.GetSemanticModelAsync(context.CancellationToken);
                if (semanticModel == null) return;

                var propertySymbol = semanticModel.GetDeclaredSymbol(propertyDecl);
                if (propertySymbol == null) return;

                context.RegisterCodeFix(
                    CodeAction.Create(
                        PropertyRule.Title.ToString(),
                        ct => new Fixer(root, propertyDecl, propertySymbol, ct).FixAsync(document),
                        equivalenceKey: PropertyRule.Id
                    ),
                    diagnostic
                );
            }
        }
    }
}
